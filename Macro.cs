// SmolScheme/Macro.cs
//
// Macros as defined in R⁷RS § 4.3.

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;

namespace SmolScheme {
    internal class SyntacticClosure : SExpr {
        public SExpr Expr;
        public readonly Env Env;

        public SyntacticClosure(SExpr expr, Env env) {
            Expr = expr;
            Env = env;
        }

        internal override SExpr ReplaceIdentifiers(Dictionary<string, SExpr> replacements) {
            return new SyntacticClosure(Expr.ReplaceIdentifiers(replacements), Env);
        }

        internal override void RecursivelyReplaceDatumLabel(int label, SExpr replacement) {
            ReplaceDatumLabel(ref Expr, label, replacement);
        }

        public override string ToString(ToStringOptions options) {
            return Expr.ToString(options);
        }

        internal override SchemeObject Quote() {
            return Expr.Quote();
        }
    }

    public abstract class Macro : Syntax {
        public Env SourceEnv;

        public Macro(Env env) {
            SourceEnv = env;
        }

        public abstract SExpr Call(SExpr[] args);

        public sealed override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                Mode mode) {
            // Wrap all tail expressions up in syntactic closures so that they get the right
            // environment.
            SExpr[] args = listExpr.Tail.Select(arg => new SyntacticClosure(arg, env)).ToArray();

            // Call.
            SExpr rawExpansion = Call(args);

            // Finally, put the result itself in a syntactic closure so that it'll be evaluated in
            // the right environment.
            SExpr expansion = new SyntacticClosure(rawExpansion, SourceEnv);
            return new Continuation(expansion, env, dynEnv, mode);
        }

        protected static ListExpr CreateCons(SExpr head, SExpr tail) {
            return new ListExpr("Cons", new SExpr[] { head, tail });
        }
    }

    public class MacroException : Exception {
        public SourceLocation Loc;

        public MacroException(string message, SourceLocation loc = null) : base(message) {
            Loc = loc;
        }
    }

    // 4.3.1 Binding constructs for syntactic keywords

    internal class LetSyntaxForm : BaseLetForm {
        public LetSyntaxForm(bool isRecursive) : base(isRecursive) { }

        protected override void PopulateEnvironment(
                Env env,
                Env subenv,
                DynamicEnv dynEnv,
                KeyValuePair<string, SExpr>[] bindings) {
            foreach (KeyValuePair<string, SExpr> pair in bindings)
                subenv.Add(pair.Key, new SyntaxDef(new SyntaxRulesMacro(pair.Value, env)));
        }
    }

    // 4.3.2 Pattern language

    internal class SyntaxRulesMacro : Macro {
        public static readonly PlaceholderDef SyntaxRules = new PlaceholderDef();
        public static readonly PlaceholderDef Ellipsis = new PlaceholderDef();
        public static readonly PlaceholderDef Underscore = new PlaceholderDef();

        private readonly KeyValuePair<ListPattern, SExpr>[] mPatterns;
        private readonly HashSet<string> mPatternLiterals;
        private readonly string mEllipsis;


        public SyntaxRulesMacro(SExpr syntaxRulesExpr, Env env) : base(env) {
            SExpr[] syntaxRulesExprs = syntaxRulesExpr.RequireListExpr(1, -1).ToArray();
            CheckArity(syntaxRulesExprs, 2, -1);

            // Unpack.
            SExpr patternLiteralExpr;
            SExpr[] syntaxRuleExprs;
            if (syntaxRulesExprs[0] is Identifier) {
                CheckArity(syntaxRulesExprs, 2, -1);
                mEllipsis = ((Identifier)syntaxRulesExprs[0]).Name;
                patternLiteralExpr = syntaxRulesExprs[1];
                syntaxRuleExprs = syntaxRulesExprs.Skip(2).ToArray();
            } else {
                mEllipsis = "...";
                patternLiteralExpr = syntaxRulesExprs[0];
                syntaxRuleExprs = syntaxRulesExprs.Skip(1).ToArray();
            }

            // Store pattern literals.
            mPatternLiterals = new HashSet<string>(patternLiteralExpr.RequireListExpr().Select(
                expr => expr.RequireIdentifier()));

            // Compile patterns.
            mPatterns = new KeyValuePair<ListPattern, SExpr>[syntaxRuleExprs.Length];
            for (int i = 0; i < mPatterns.Length; i++) {
                SExpr[] syntaxRulePairExprs = syntaxRuleExprs[i].RequireListExpr(2, 2).ToArray();

                ListPattern pattern = ListPattern.CompileOutermost(
                    syntaxRulePairExprs[0], env, mPatternLiterals, mEllipsis);
                mPatterns[i] = new KeyValuePair<ListPattern, SExpr>(
                    pattern, syntaxRulePairExprs[1]);
            }
        }

        public override SExpr Call(SExpr[] argsArray) {
            // The first element of the list is immaterial, so just set it to null before we match.
            ListExpr listExpr = new ListExpr(SchemeNull.Null, argsArray);
            foreach (KeyValuePair<ListPattern, SExpr> pair in mPatterns) {
                var bindings = new Dictionary<string, SExpr>();
                if (pair.Key.Matches(listExpr, null, bindings))
                    return pair.Value.ReplaceIdentifiers(bindings);
            }

            throw new SchemeException("No pattern matched the input: " + listExpr);
        }
    }

    internal abstract class Pattern {
        protected abstract bool MatchesInner(
            SExpr expr, Env env, Dictionary<string, SExpr> bindings);

        public bool Matches(SExpr expr, Env env, Dictionary<string, SExpr> bindings) {
            if (expr is SyntacticClosure) {
                var syntacticClosure = (SyntacticClosure)expr;
                return Matches(syntacticClosure.Expr, syntacticClosure.Env, bindings);
            }

            return MatchesInner(expr, env, bindings);
        }

        protected static Pattern Compile(
                SExpr expr, Env env, HashSet<string> literals, string ellipsis) {
            if (expr is Identifier) {
                Identifier ident = (Identifier)expr;
                if (env.RefersTo(ident.Name, SyntaxRulesMacro.Underscore))
                    return new WildcardPattern();
                if (literals.Contains(ident.Name))
                    return new LiteralIdentifierPattern(ident.Name, env);
                return new NonLiteralIdentifierPattern(ident.Name);
            }

            if (expr is ListExpr)
                return ListPattern.CompileInner(expr, env, literals, ellipsis);
            if (expr is SplatExpr)
                return SplatPattern.CompileInner(expr, env, literals, ellipsis);
            if (expr is VectorExpr)
                return VectorPattern.CompileInner(expr, env, literals, ellipsis);
            if (expr is SchemeObject)
                return new ConstantPattern((SchemeObject)expr);

            throw new SchemeException(
                "This expression of type " + expr.GetType() + " isn't a pattern");
        }
    }

    internal class WildcardPattern : Pattern {
        internal WildcardPattern() { }

        protected override bool MatchesInner(
                SExpr expr, Env env, Dictionary<string, SExpr> bindings) {
            return true;
        }
    }

    internal class NonLiteralIdentifierPattern : Pattern {
        private string mName;

        internal NonLiteralIdentifierPattern(string name) {
            mName = name;
        }

        protected override bool MatchesInner(
                SExpr expr, Env env, Dictionary<string, SExpr> bindings) {
            bindings.Add(mName, expr);
            return true;
        }
    }

    internal class LiteralIdentifierPattern : Pattern {
        private string mName;
        private Def mDef;

        public LiteralIdentifierPattern(string name, Env env) {
            mName = name;
            mDef = env.Get(name);
        }

        protected override bool MatchesInner(
                SExpr expr, Env env, Dictionary<string, SExpr> bindings) {
            if (!(expr is Identifier))
                return false;

            Identifier identifier = (Identifier)expr;
            if (mDef == null)
                return mName == identifier.Name && env.Get(mName) == null;
            return env.RefersTo(identifier, mDef);
        }
    }

    /// <summary>
    /// Base class of list, splat, and vector patterns.
    /// </summary>
    internal abstract class SequencePattern<E> : Pattern where E: SExpr, IEnumerable<SExpr> {
        protected readonly Pattern[] mPrefix;
        protected readonly Pattern[] mSuffix;

        protected SequencePattern(Pattern[] prefix, Pattern[] suffix) {
            mPrefix = prefix;
            mSuffix = suffix;
        }

        protected static List<Pattern> CreateSubpatterns(
                SExpr expr,
                Env env,
                HashSet<string> literals,
                string ellipsis,
                out List<Pattern> outSuffix) {
            if (!(expr is E))
                throw new SchemeException("Pattern must be a " + typeof(E));

            List<Pattern> prefix = new List<Pattern>();
            outSuffix = null;

            foreach (SExpr subexpr in (E)expr) {
                if (outSuffix != null)
                    outSuffix.Add(Pattern.Compile(subexpr, env, literals, ellipsis));
                else if (env.RefersTo(subexpr, SyntaxRulesMacro.Ellipsis))
                    outSuffix = new List<Pattern>();
                else
                    prefix.Add(Pattern.Compile(subexpr, env, literals, ellipsis));
            }

            return prefix;
        }

        protected override bool MatchesInner(
                SExpr expr, Env env, Dictionary<string, SExpr> bindings) {
            // Check that the expression is a list.
            if (!(expr is E))
                return false;
            var listExpr = (E)expr;

            // Check length if it must be exact.
            int length = listExpr.Count();
            if (mSuffix == null && mPrefix.Length != length)
                return false;

            // Check prefix.
            if (length > mPrefix.Length)
                return false;
            for (int i = 0; i < length; i++) {
                if (!mPrefix[i].Matches(listExpr.ElementAt(i), env, bindings))
                    return false;
            }

            // Check suffix, if applicable.
            if (mSuffix != null) {
                if (length < mPrefix.Length + mSuffix.Length)
                    return false;
                for (int i = 0; i < mSuffix.Length; i++) {
                    int listIndex = length - mSuffix.Length + i;
                    if (!mSuffix[i].Matches(listExpr.ElementAt(listIndex), env, bindings))
                        return false;
                }
            }

            return true;
        }
    }

    internal class ListPattern : SequencePattern<ListExpr> {
        public ListPattern(Pattern[] prefix, Pattern[] suffix) : base(prefix, suffix) { }

        public static ListPattern CompileInner(
                SExpr expr, Env env, HashSet<string> literals, string ellipsis) {
            List<Pattern> suffix;
            List<Pattern> prefix = CreateSubpatterns(expr, env, literals, ellipsis, out suffix);
            return new ListPattern(
                prefix == null ? null : prefix.ToArray(),
                suffix == null ? null : suffix.ToArray());
        }

        public static ListPattern CompileOutermost(
                SExpr expr, Env env, HashSet<string> literals, string ellipsis) {
            ListPattern pattern = ListPattern.CompileInner(expr, env, literals, ellipsis);
            // The head of the list is immaterial, so just replace it with a wildcard.
            pattern.mPrefix[0] = new WildcardPattern();
            return pattern;
        }
    }

    /// <summary>
    /// A pattern for improper lists.
    /// </summary>
    internal class SplatPattern : SequencePattern<SplatExpr> {
        private Pattern mTail;

        public SplatPattern(Pattern[] prefix, Pattern[] suffix, Pattern tail) :
                base(prefix, suffix) {
            mTail = tail;
        }

        public static SplatPattern CompileInner(
                SExpr expr, Env env, HashSet<string> literals, string ellipsis) {
            List<Pattern> suffix;
            List<Pattern> prefix = CreateSubpatterns(expr, env, literals, ellipsis, out suffix);
            // If the above succeeded, we know we have a SplatExpr, so we can safely downcast.
            Pattern tailPattern = Pattern.Compile(((SplatExpr)expr).Tail, env, literals, ellipsis);
            return new SplatPattern(
                prefix == null ? null : prefix.ToArray(),
                suffix == null ? null : suffix.ToArray(),
                tailPattern);
        }

        protected override bool MatchesInner(
                SExpr expr, Env env, Dictionary<string, SExpr> bindings) {
            // If the above succeeded, we know we have a SplatExpr, so we can safely downcast.
            return mTail.Matches(((SplatExpr)expr).Tail, env, bindings);
        }
    }

    internal class VectorPattern : SequencePattern<VectorExpr> {
        public VectorPattern(Pattern[] prefix, Pattern[] suffix) : base(prefix, suffix) {}

        public static VectorPattern CompileInner(
                SExpr expr, Env env, HashSet<string> literals, string ellipsis) {
            List<Pattern> suffix;
            List<Pattern> prefix = CreateSubpatterns(expr, env, literals, ellipsis, out suffix);
            return new VectorPattern(
                prefix == null ? null : prefix.ToArray(),
                suffix == null ? null : suffix.ToArray());
        }
    }

    internal class ConstantPattern : Pattern {
        SchemeObject mDatum;

        public ConstantPattern(SchemeObject datum) {
            mDatum = datum;
        }

        protected override bool MatchesInner(
                SExpr expr, Env env, Dictionary<string, SExpr> bindings) {
            return expr is SchemeObject && ((SchemeObject)expr).Equal(mDatum);
        }
    }

    // 4.3.3 Signaling errors in macro transformers

    internal class SyntaxErrorForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                Mode mode) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 1, -1);

            var extraInfo = args.Skip(1).Select(arg => arg.Quote()).ToArray();
            var error = new Error(args[0].RequireString(), extraInfo);
            throw new SchemeException(error);
        }
    }
}
