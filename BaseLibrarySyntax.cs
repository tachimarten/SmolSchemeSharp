// SmolScheme/BaseLibrarySyntax.cs
//
// Syntax defined in R⁷RS, §§ 4.1 and 4.2.

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;

namespace SmolScheme {
    // 4 Expressions

    // 4.1.4 Procedures

    internal class LambdaForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] formArgs = GetArguments(listExpr);
            CaseLambdaClause clause = CaseLambdaForm.BuildClause(formArgs);
            return new Result(new SchemeProcDatum(clause, env));
        }
    }

    // 4.1.5 Conditionals

    internal class IfMacro : Macro {
        public override SExpr Call(SExpr[] args) {
            CheckArity(args, 2, 3);
            return new ListExpr("Cond", new SExpr[] {
                new ListExpr(args[0], new SExpr[] { args[1] }),
                new ListExpr("Else", new SExpr[] {
                    args.Length > 2 ? args[2] : SchemeNull.Null
                }),
            });
        }
    }

    // 4.1.6 Assignments

    internal class SetForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 2);

            string name = args[0].RequireIdentifier();
            Def def = env.Get(name);
            if (!(def is ObjectDef))
                throw new SchemeException("\"" + name + "\" doesn't name a value in this scope");

            ((ObjectDef)def).Value = Interpreter.Current.Evaluate(args[1], env, dynEnv);
            return new Result(SchemeNull.Null);
        }
    }

    // 4.1.7 Inclusion

    internal class IncludeForm : Syntax {
        private bool mCaseInsensitive;

        public IncludeForm(bool caseInsensitive) {
            mCaseInsensitive = caseInsensitive;
        }

        // Extracted here so that we can use it for the identically-named library declarations.
        internal static SExpr[] Include(ListExpr listExpr, bool caseInsensitive) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 1, -1);

            SExpr[] results = new SExpr[args.Length];
            for (int i = 0; i < args.Length; i++) {
                string path = args[i].RequireString();
                MJSReader reader = new MJSReader(path);
                results[i] = reader.ParseSExpr();
            }
            return results;
        }

        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] results = Include(listExpr, mCaseInsensitive);
            ListExpr result = new ListExpr("Begin", results, listExpr.Loc);
            return new Result(Interpreter.Current.Evaluate(result, env, dynEnv, allowDefine));
        }
    }

    // 4.2 Derived expression types

    // 4.2.1 Conditionals

    internal class CondForm : Syntax {
        public static readonly PlaceholderDef Arrow = new PlaceholderDef();
        public static readonly PlaceholderDef Else = new PlaceholderDef();

        // This is factored out so that GuardForm can use it too.
        internal static Evaluation EvaluateCond(SExpr[] clauses, Env env, DynamicEnv dynEnv) {
            for (int i = 0; i < clauses.Length; i++) {
                SExpr clause = clauses[i];
                if (!(clause is ListExpr))
                    throw new SchemeException("`Cond` clause must be a list");
                var listClause = (ListExpr)clause;

                SchemeObject condResult = null;
                if (!env.RefersTo(listClause.Head, Else)) {
                    condResult = Interpreter.Current.Evaluate(listClause.Head, env, dynEnv);
                    if (condResult.IsFalsy)
                        continue;
                } else {
                    // Check if `Else` is last.
                    if (i + 1 != clauses.Length)
                        throw new SchemeException("`Else` is only allowed in the last clause");
                }

                if (listClause.Length > 1 && env.RefersTo(listClause[1], Arrow)) {
                    if (listClause.Length != 3) {
                        throw new SchemeException(
                            "Expected a list with exactly 3 values when using the `=>` form");
                    }

                    SchemeProc proc = Interpreter.Current.Evaluate(
                        listClause[2], env, dynEnv).Require<SchemeProc>();
                    ListExpr call = new ListExpr(proc, new SchemeObject[] { condResult });
                    return new Continuation(call, env, dynEnv, false);
                }

                return EvaluateMultiple(listClause.Tail, env, dynEnv, false);
            }

            return null;
        }

        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] args = GetArguments(listExpr);
            Evaluation evaluation = EvaluateCond(args, env, dynEnv);
            return evaluation == null ? new Result(SchemeNull.Null) : evaluation;
        }
    }

    internal class CaseForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 2, -1);

            SchemeObject key = Interpreter.Current.Evaluate(args[0], env, dynEnv);

            for (int i = 1; i < args.Length; i++) {
                SExpr clause = args[i];
                IEnumerable<SExpr> clausePieces = clause.RequireListExpr(2, -1);

                bool matched = false;
                SExpr head = clausePieces.First();
                if (env.RefersTo(head, CondForm.Else)) {
                    // Check if `Else` is last.
                    if (i + 1 != args.Length)
                        throw new SchemeException("`Else` is only allowed in the last clause");
                    matched = true;
                } else {
                    foreach (SExpr query in head.RequireListExpr(1, -1)) {
                        if (query.Quote().Eqv(key)) {
                            matched = true;
                            break;
                        }
                    }
                }

                if (!matched)
                    continue;

                // Handle `=>` form.
                if (clausePieces.Count() > 1 &&
                        env.RefersTo(clausePieces.ElementAt(1), CondForm.Arrow)) {
                    if (clausePieces.Count() != 3)
                        throw new SchemeException("`=>` must be followed by exactly one argument");
                    SchemeProc proc = clausePieces.ElementAt(2).Require<SchemeProc>();
                    return new Continuation(
                        new ListExpr(proc, new SExpr[] { key }), new Env(), dynEnv, false);
                }

                return EvaluateMultiple(clausePieces.Skip(1), env, dynEnv, false);
            }

            return new Result(SchemeNull.Null);
        }
    }

    internal class AndForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            foreach (SExpr arg in listExpr.Tail) {
                if (!NativeProc.RequireBool(Interpreter.Current.Evaluate(arg, env, dynEnv)))
                    return new Result(SchemeBool.False);
            }
            return new Result(SchemeBool.True);
        }
    }

    internal class OrForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            foreach (SExpr arg in listExpr.Tail) {
                if (NativeProc.RequireBool(Interpreter.Current.Evaluate(arg, env, dynEnv)))
                    return new Result(SchemeBool.True);
            }
            return new Result(SchemeBool.False);
        }
    }

    internal class WhenMacro : Macro {
        public override SExpr Call(SExpr[] args) {
            CheckArity(args, 2, -1);
            return new ListExpr("Cond", new SExpr[] {
                new ListExpr(args[0], args.Skip(1).ToArray()),
            });
        }
    }

    internal class UnlessMacro : Macro {
        public override SExpr Call(SExpr[] args) {
            CheckArity(args, 2, -1);
            return new ListExpr("Cond", new SExpr[] {
                new ListExpr(args[0], new SExpr[] { SchemeNull.Null }),
                new ListExpr("Else", args.Skip(1).ToArray()),
            });
        }
    }

    // 4.2.2 Binding constructs

    // Let, LetSyntax, Letrec, Let*, Letrec*, and LetrecSyntax.
    internal abstract class BaseLetForm : Syntax {
        protected bool mIsRecursive;

        protected BaseLetForm(bool isRecursive) {
            mIsRecursive = isRecursive;
        }

        protected abstract void PopulateEnvironment(
            Env env,
            Env subenv,
            DynamicEnv dynEnv,
            KeyValuePair<string, SExpr>[] bindings);

        public sealed override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 2, -1);
            SExpr[] bindingsExpr = args[0].RequireListExpr().ToArray();

            var bindings = new KeyValuePair<string, SExpr>[bindingsExpr.Length];
            for (int i = 0; i < bindingsExpr.Length; i++) {
                SExpr[] bindingPieces = bindingsExpr[i].RequireListExpr(2).ToArray();
                string symbolName = bindingPieces[0].RequireIdentifier();
                bindings[i] = new KeyValuePair<string, SExpr>(symbolName, bindingPieces[1]);
            }

            Env subenv = new Env(env);
            if (mIsRecursive) {
                foreach (KeyValuePair<string, SExpr> pair in bindings)
                    subenv.Declare(pair.Key);
            }

            PopulateEnvironment(env, subenv, dynEnv, bindings);

            return EvaluateMultiple(args.Skip(1), subenv, dynEnv, true);
        }
    }

    // Let, Letrec, Let*, and Letrec*.
    internal class LetForm : BaseLetForm {
        private bool mIsStar;

        public LetForm(bool isRecursive, bool isStar) : base(isRecursive) {
            mIsStar = isStar;
        }

        protected override void PopulateEnvironment(
                Env env,
                Env subenv,
                DynamicEnv dynEnv,
                KeyValuePair<string, SExpr>[] bindings) {
            // If star form, add bindings straight away.
            SchemeObject[] stagingValues = new SchemeObject[bindings.Length];
            for (int i = 0; i < bindings.Length; i++) {
                KeyValuePair<string, SExpr> binding = bindings[i];
                SchemeObject symbolValue =
                    Interpreter.Current.Evaluate(binding.Value, subenv, dynEnv);
                stagingValues[i] = symbolValue;

                if (mIsStar)
                    subenv.Add(binding.Key, symbolValue);
            }

            // If not the star form, add the bindings all at once at the end.
            if (!mIsStar) {
                for (int i = 0; i < bindings.Length; i++)
                    subenv.Add(bindings[i].Key, stagingValues[i]);
            }
        }
    }

    // LetValues and Let*Values.
    internal class LetValuesForm : Syntax {
        private bool mIsStar;

        public LetValuesForm(bool isStar) {
            mIsStar = isStar;
        }

        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 2, -1);
            SExpr[] bindingsExpr = args[0].RequireListExpr().ToArray();

            var bindings = new KeyValuePair<string[], SExpr>[bindingsExpr.Length];
            for (int i = 0; i < bindingsExpr.Length; i++) {
                SExpr[] bindingPieces = bindingsExpr[i].RequireListExpr(2).ToArray();
                // TODO: Do the formals have to be a list? Check another implementation.
                SExpr[] formals = bindingPieces[0].RequireListExpr().ToArray();
                string[] symbolNames =
                    formals.Select(formal => formal.RequireIdentifier()).ToArray();
                bindings[i] = new KeyValuePair<string[], SExpr>(symbolNames, bindingPieces[1]);
            }

            Env subenv = new Env(env);

            SchemeObject[][] stagingValues = new SchemeObject[bindingsExpr.Length][];
            for (int i = 0; i < bindingsExpr.Length; i++) {
                KeyValuePair<string[], SExpr> binding = bindings[i];
                SchemeObject[] symbolValues = Interpreter.Current.EvaluateMulti(
                    binding.Value, subenv, dynEnv, false, binding.Key.Length);
                stagingValues[i] = symbolValues;

                if (mIsStar) {
                    // We already checked for arity in `EvaluateMulti`.
                    for (int j = 0; j < symbolValues.Length; j++)
                        subenv.Add(binding.Key[j], symbolValues[j]);
                }
            }

            // If not the star form, add the bindings now.
            if (!mIsStar) {
                for (int i = 0; i < bindingsExpr.Length; i++) {
                    string[] binding = bindings[i].Key;
                    SchemeObject[] values = stagingValues[i];
                    for (int j = 0; j < values.Length; j++)
                        subenv.Add(binding[j], values[j]);
                }
            }

            return EvaluateMultiple(args.Skip(1), subenv, dynEnv, true);
        }
    }

    // 4.2.1 Conditionals

    internal class CondExpandForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SourceLocation loc;
            IEnumerable<SExpr> expansion = EvaluateForm(listExpr, env, out loc);

            if (expansion != null) {
                SExpr beginExpr = new ListExpr("Begin", expansion.ToArray(), loc);
                return new Continuation(beginExpr, env, dynEnv, allowDefine);
            }

            return new Result(SchemeNull.Null);
        }

        /// <summary>
        /// Generic version, for use in both the normal `CondExpand` syntax and the `DefineLibrary`
        /// syntax.
        /// </summary>
        internal static IEnumerable<SExpr> EvaluateForm(
                ListExpr listExpr, Env env, out SourceLocation outLoc) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 1, -1);

            for (int i = 0; i < args.Length; i++) {
                SExpr ceClauseExpr = args[i];
                IEnumerable<SExpr> ceClause = ceClauseExpr.RequireListExpr(1, -1);

                // Require that Else be the last clause.
                bool isElse = env.RefersTo(ceClause.First(), CondForm.Else);
                if (isElse && i + 1 != args.Length)
                    throw new SchemeException("`Else` is only allowed on the last clause");

                if (isElse || EvaluateFeatureRequirement(ceClause.First())) {
                    outLoc = ceClauseExpr.Loc;
                    return ceClause.Skip(1);
                }
            }

            outLoc = null;
            return null;
        }

        private static bool EvaluateFeatureRequirement(SExpr featureRequirementExpr) {
            if (featureRequirementExpr is Identifier)
                return Interpreter.Features.Contains(((Identifier)featureRequirementExpr).Name);

            if (!(featureRequirementExpr is ListExpr)) {
                throw new SchemeException(
                    "Feature requirement must be either an identifier or a list");
            }

            SExpr[] featureRequirement = featureRequirementExpr.RequireListExpr(1, -1).ToArray();

            if (featureRequirement[0] is Identifier) {
                switch (((Identifier)featureRequirement[0]).Name) {
                    case "Library":
                        LibraryName libraryName = new LibraryName(featureRequirement.Skip(1).Select(
                            expr => expr.RequireIdentifier()).ToArray());

                        return Interpreter.Current.Libraries.LoadLibrary(libraryName) != null;

                    case "And":
                        return featureRequirement.Skip(1).All(
                            expr => EvaluateFeatureRequirement(expr));

                    case "Or":
                        return featureRequirement.Skip(1).Any(
                            expr => EvaluateFeatureRequirement(expr));

                    case "Not":
                        if (featureRequirement.Length != 2) {
                            throw new SchemeException(
                                "A Not feature requirement requires exactly 1 argument");
                        }
                        return EvaluateFeatureRequirement(featureRequirement[1]);
                }
            }

            throw new SchemeException(
                "A feature requirement must either be an identifier or begin with Library, And, " +
                "Or, or Not");
        }
    }

    // 4.2.3 Sequencing

    internal class BeginForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            return EvaluateMultiple(listExpr.Tail, env, dynEnv, allowDefine);
        }
    }

    // 4.2.4 Iteration

    internal class DoForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 2, -1);

            Initializer[] initializers = args[0].RequireListExpr().Select(
                initializer => new Initializer(initializer)).ToArray();
            IEnumerable<SExpr> testClause = args[1].RequireListExpr();
            SExpr test = testClause.First();
            SExpr[] finalExprs = testClause.Skip(1).ToArray();

            // Set up.
            Env subenv = new Env(env);
            foreach (Initializer initializer in initializers) {
                SchemeObject value = Interpreter.Current.Evaluate(
                    initializer.Init, env, dynEnv, false);
                subenv.Add(initializer.Name, value);
            }

            // Iterate.
            while (!Interpreter.Current.Evaluate(test, subenv, dynEnv).IsTruthy) {
                foreach (SExpr command in args.Skip(2))
                    Interpreter.Current.Evaluate(command, subenv, dynEnv);
                foreach (Initializer initializer in initializers) {
                    if (initializer.Step != null) {
                        SchemeObject initialValue = Interpreter.Current.Evaluate(
                            initializer.Step, subenv, dynEnv);
                        subenv.Add(initializer.Name, initialValue);
                    }
                }
            }

            // Finish up.
            for (int i = 0; i < finalExprs.Length - 1; i++)
                Interpreter.Current.Evaluate(finalExprs[i], subenv, dynEnv);
            return new Continuation(finalExprs[finalExprs.Length - 1], subenv, dynEnv, false);
        }

        private class Initializer {
            public string Name;
            public SExpr Init;
            public SExpr Step;

            public Initializer(SExpr initializer) {
                SExpr[] initializerParts = initializer.RequireListExpr(2, 3).ToArray();
                Name = initializerParts[0].RequireIdentifier();
                Init = initializerParts[1];
                Step = initializerParts.Length > 2 ? initializerParts[2] : null;
            }
        }
    }

    // 4.2.6 Dynamic bindings

    public class Parameter : NativeProc {
        public Parameter() { }

        [SchemeBridge("MakeParameter")]
        public Parameter(SchemeObject init, Func<SchemeObject, SchemeObject> converter = null) {
            SimpleNativeProc.CurrentDynamicEnv[this] = converter == null ? init : converter(init);
        }

        public override Evaluation Call(
                SchemeObject[] args,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            return new Result(dynEnv[this]);
        }

        public override int GetHashCode() {
            return (int)mID;
        }

        public override bool Equals(object obj) {
            return this == obj;
        }

        /// <summary>
        /// NB: You may only use this within a SimpleNativeProc.
        /// </summary>
        public SchemeObject Get() {
            return SimpleNativeProc.CurrentDynamicEnv[this];
        }
    }

    internal class ParameterizeForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 2, -1);

            SExpr[] bindings = args[0].RequireListExpr().ToArray();

            DynamicEnv dynSubenv = new DynamicEnv(dynEnv);

            for (int i = 0; i < bindings.Length; i++) {
                SExpr pairExpr = bindings[i];
                SExpr[] binding = pairExpr.RequireListExpr(2).ToArray();

                Parameter parameter =
                    Interpreter.Current.Evaluate(binding[0], env, dynEnv).Require<Parameter>();
                SchemeObject value = Interpreter.Current.Evaluate(binding[1], env, dynEnv);

                dynSubenv[parameter] = value;
            }

            return EvaluateMultiple(args.Skip(2), env, dynSubenv, true);
        }
    }

    // 4.2.7 Exception handling

    internal class GuardForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 2);

            GuardHandlerProc guardHandler = new GuardHandlerProc(args[0], dynEnv);
            DynamicEnv dynSubenv = new DynamicEnv(
                dynEnv,
                new ExceptionHandler(guardHandler, dynEnv.ExceptionHandler));

            try {
                Env subenv = new Env(env);
                SchemeObject result = SchemeNull.Null;
                foreach (SExpr expr in args.Skip(1))
                    result = Interpreter.Current.Evaluate(expr, subenv, dynSubenv, true);
                return new Result(result);
            } catch (SchemeException exception) {
                return guardHandler.Call(
                    new SchemeObject[1] { exception.Error }, env, dynEnv, false);
            }
        }

        private class GuardHandlerProc : NativeProc {
            private string mExceptionSymbol;
            private SExpr[] mClauses;
            // The dynamic environment of the Guard expression.
            private DynamicEnv mGuardDynEnv;

            public GuardHandlerProc(SExpr arg, DynamicEnv guardDynEnv) {
                IEnumerable<SExpr> handlerExprs = arg.RequireListExpr(1);
                mExceptionSymbol = handlerExprs.First().RequireIdentifier();
                mClauses = handlerExprs.Skip(1).ToArray();

                mGuardDynEnv = guardDynEnv;
            }

            public override Evaluation Call(
                    SchemeObject[] args,
                    Env env,
                    DynamicEnv dynEnv,
                    bool allowDefine) {
                SchemeObject exception = args[0];

                Env subenv = new Env(env);
                subenv.Add(mExceptionSymbol, exception);

                Evaluation evaluation = CondForm.EvaluateCond(mClauses, subenv, mGuardDynEnv);
                if (evaluation != null)
                    return evaluation;

                // FIXME: Is this right?
                return new Result(mGuardDynEnv.RaiseContinuable(exception));
            }
        }
    }

    // 4.2.8 Quasiquotation

    internal class QuasiquoteForm : Syntax {
        public static readonly PlaceholderDef Unquote = new PlaceholderDef();
        public static readonly PlaceholderDef UnquoteSplicing = new PlaceholderDef();

        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 1);

            SchemeObject[] quasiquoted = Quasiquote(args[0], env, dynEnv);
            if (quasiquoted.Length > 1)
                throw new SchemeException("UnquoteSplicing[] may only be used in a list");
            return new Result(quasiquoted[0]);
        }

        private SchemeObject[] Quasiquote(SExpr expr, Env env, DynamicEnv dynEnv) {
            // Handle list expressions.
            if (expr is ListExpr) {
                var listExpr = (ListExpr)expr;

                // Handle `Unquote`.
                if (env.RefersTo(listExpr.Head, Unquote)) {
                    if (listExpr.Tail.Length != 1)
                        throw new SchemeException("Unquote[] requires exactly one argument");
                    return new SchemeObject[] {
                        Interpreter.Current.Evaluate(listExpr.Tail[0], env, dynEnv),
                    };
                }

                // Handle `UnquoteSplicing`.
                if (env.RefersTo(listExpr.Head, UnquoteSplicing)) {
                    return listExpr.Tail.Select(
                        subexpr => Interpreter.Current.Evaluate(subexpr, env, dynEnv)).ToArray();
                }

                // Handle `Quote` and `Quasiquote`.
                if (env.RefersToSyntax<QuoteForm>(listExpr.Head) ||
                        env.RefersToSyntax<QuasiquoteForm>(listExpr.Head)) {
                    return new SchemeObject[] {
                        Interpreter.Current.Evaluate(listExpr, env, dynEnv)
                    };
                }

                var newElements = new List<SchemeObject>();
                foreach (SExpr subexpr in listExpr)
                    newElements.AddRange(Quasiquote(subexpr, env, dynEnv));
                return new SchemeObject[] { SchemeList.CreateList(newElements) };
            }

            // Handle splat expressions (improper lists).
            if (expr is SplatExpr) {
                var splatExpr = (SplatExpr)expr;

                var newElements = new List<SchemeObject>();
                newElements.AddRange(Quasiquote(splatExpr.Head, env, dynEnv));
                foreach (SExpr subexpr in splatExpr.Body)
                    newElements.AddRange(Quasiquote(subexpr, env, dynEnv));

                SchemeObject[] tails = Quasiquote(splatExpr.Tail, env, dynEnv);
                if (tails.Length != 1) {
                    throw new SchemeException(
                        "UnquoteSplicing[] may not be used at the end of an improper list");
                }
                if (newElements.Count == 0) {
                    throw new SchemeException(
                        "This use of UnquoteSplicing[] resulted in an invalid list");
                }

                return new SchemeObject[] {
                    new SchemeList(newElements[0], newElements.Skip(1).ToArray(), tails[0])
                };
            }

            if (expr is Identifier)
                return new SchemeObject[] { expr.Quote() };

            if (expr is SchemeObject)
                return new SchemeObject[] { (SchemeObject)expr };

            throw new SchemeException("Internal error: Unhandled expression type in `Quasiquote()`: " +
                expr.GetType());
        }
    }

}
