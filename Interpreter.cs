// SmolScheme/Interpreter.cs

using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading;

namespace SmolScheme {
    // Interpreter

    public class Interpreter {
        private static ThreadLocal<Interpreter> sCurrentInterpreter =
            new ThreadLocal<Interpreter>();

        public readonly static HashSet<string> Features = new HashSet<string> {
            "IeeeFloat",
            "SmolScheme",
            "SmolScheme0.1",
            "NoUndelimitedContinuations",
            "NoMultiShotContinuations",
        };

        public readonly Libraries Libraries;

        private readonly bool mPreferMJSForm;

        public static Interpreter Current {
            get { return sCurrentInterpreter.Value; }
        }

        /// <summary>
        /// If `preferMJSForm` is true, then symbols in error messages are printed in MJS form;
        /// otherwise, they're printed in S form. This has no effect on anything other than error
        /// messages.
        /// </summary>
        public Interpreter(Libraries libraries, bool preferMJSForm = false) {
            Libraries = libraries;
            mPreferMJSForm = preferMJSForm;
        }

        /// <summary>
        /// Loads and runs a Scheme program.
        /// </summary>
        public void Load(
                Stream stream,
                Env env,
                DynamicEnv dynEnv,
                string path = null,
                bool useMJSReader = false) {
            using (var streamReader = new StreamReader(new BufferedStream(stream))) {
                Reader reader = new InitialFileLexer(streamReader).CreateReader();

                Mode mode = Mode.AllowDefine | Mode.AllowImports;
                try {
                    while (true) {
                        reader.AdvanceToNextTopLevelStatement();
                        SExpr expr = reader.ParseSExpr();
                        if (expr == null) {
                            reader.ExpectEof("expression or end of file");
                            break;
                        }

                        // According to R⁷RS § 5.1, imports are only allowed at the start of the
                        // file.
                        //
                        // NB: In § 5.6.2, R⁷RS gives an example of a program that uses imports
                        // after `DefineLibrary`, in apparent contradiction to the earlier spec
                        // language. Because allowing imports after `DefineLibrary` is useful, I've
                        // chosen to allow it.
                        if (!(expr is ListExpr)) {
                            SExpr head = ((ListExpr)expr).Head;
                            if (!env.RefersToSyntax<ImportForm>(head) &&
                                    !env.RefersToSyntax<DefineLibraryForm>(head)) {
                                mode &= ~Mode.AllowImports;
                            }
                        }

                        Evaluate(expr, env, dynEnv, mode);
                    }
                } finally {
                    reader.Dispose();
                }
            }
        }

        public void Load(Env env, DynamicEnv dynEnv, string path, bool useMJSReader = false) {
            Load(File.Open(path, FileMode.Open), env, dynEnv, path, useMJSReader);
        }

        /// <summary>
        /// Evaluates an expression returning a single value.
        /// </summary>
        public SchemeObject Evaluate(
                SExpr expr, Env env, DynamicEnv dynEnv, Mode mode = Mode.Default) {
            return EvaluateMulti(expr, env, dynEnv, mode, 1)[0];
        }

        /// <summary>
        /// Evaluates an expression returning multiple values.
        /// </summary>
        public SchemeObject[] EvaluateMulti(
                SExpr expr,
                Env env,
                DynamicEnv dynEnv,
                Mode mode = Mode.Default,
                int expectedValueCount = -1) {
            Interpreter oldInterpreter = sCurrentInterpreter.Value;
            sCurrentInterpreter.Value = this;

            // Is this the outermost evaluation of a procedure call?
            bool isProc = false;

            try {
                while (true) {
                    Evaluation evaluation = EvaluateOnce(expr, env, dynEnv, mode);
                    if (evaluation is Continuation) {
                        var continuation = (Continuation)evaluation;
                        expr = continuation.Expr;
                        env = continuation.Env;
                        dynEnv = continuation.DynEnv;
                        mode = continuation.Mode;
                        isProc = continuation.IsProc;
                        continue;
                    }

                    Result result = (Result)evaluation;
                    if (expectedValueCount >= 0 && expectedValueCount != result.Datums.Length) {
                        throw new SchemeException("Expected " +
                            SExprUtils.Pluralize("value", expectedValueCount) +
                            ", but this expression evaluated to " +
                            SExprUtils.Pluralize("value", result.Datums.Length));
                    }

                    return result.Datums;
                }
            } catch (SchemeException exception) {
                exception.AttachLocation(expr.Loc);
                if (isProc)
                    exception.PushStackFrame(null);
                throw;
            } finally {
                sCurrentInterpreter.Value = oldInterpreter;
            }
        }

        private Evaluation EvaluateOnce(SExpr expr, Env env, DynamicEnv dynEnv, Mode mode) {
            // Values are self-evaluating.
            if (expr is SchemeObject)
                return new Result((SchemeObject)expr);

            // Look up any symbols.
            if (expr is Identifier) {
                string name = ((Identifier)expr).Name;
                Def def = env.Get(name);
                if (def == null)
                    throw new SchemeException("Undefined symbol: " + name);
                if (def is PlaceholderDef)
                    throw new SchemeException("Symbol hasn't been defined yet: " + name);
                if (def is ObjectDef)
                    return new Result(((ObjectDef)def).Value);
                throw new SchemeException("Symbol isn't a datum: " + name);
            }

            // Evaluate lists.
            if (expr is ListExpr)
                return EvaluateList((ListExpr)expr, env, dynEnv, mode);

            // Evaluate vector and bytevector expressions.
            if (expr is BaseVectorExpr) {
                SchemeObject[] elements = ((BaseVectorExpr)expr).Select(
                    element => Evaluate(element, env, dynEnv)).ToArray();
                if (expr is VectorExpr)
                    return new Result(new Vector(elements));
                if (expr is BytevectorExpr) {
                    return new Result(new Bytevector(elements.Select(
                        elem => elem.RequireExactByte()).ToArray()));
                }
                throw new Exception("Unhandled vector expression type");
            }

            // Evaluate syntactic closures.
            if (expr is SyntacticClosure) {
                var closure = (SyntacticClosure)expr;
                return new Continuation(closure.Expr, closure.Env, dynEnv, mode);
            }

            throw new SchemeException("Can't evaluate an S-expression of type: " + expr.GetType());
        }

        private Evaluation EvaluateList(
                ListExpr listExpr, Env env, DynamicEnv dynEnv, Mode mode) {
            // Handle syntax.
            if (listExpr.Head is Identifier) {
                Def def = env.Get(listExpr.Head.RequireIdentifier());
                if (def != null && def is SyntaxDef) {
                    var syntax = ((SyntaxDef)def).Value;
                    return syntax.Evaluate(listExpr, env, dynEnv, mode);
                }
            }

            // Normal evaluation.
            SchemeObject head = Evaluate(listExpr.Head, env, dynEnv);
            SchemeObject[] args = listExpr.Tail.Select(arg => Evaluate(arg, env, dynEnv)).ToArray();

            // Handle function calls.
            if (head is SchemeProcDatum) {
                var fun = (SchemeProcDatum)head;

                Env rib = new Env(fun.Env);
                SExpr body = null;
                for (int clauseIndex = 0; clauseIndex < fun.Clauses.Length; clauseIndex++) {
                    CaseLambdaClause clause = fun.Clauses[clauseIndex];
                    bool isLast = clauseIndex + 1 == fun.Clauses.Length;

                    if (clause.Args is NormalProcArgs) {
                        string[] funArgs = ((NormalProcArgs)clause.Args).Args;
                        if (!SExprUtils.CheckArity(args.Length, funArgs.Length, funArgs.Length)) {
                            if (isLast) {
                                SExprUtils.ReportArityError(
                                    "procedure",
                                    args.Length,
                                    funArgs.Length,
                                    funArgs.Length,
                                    listExpr.Loc);
                            }
                            continue;
                        }

                        for (int i = 0; i < funArgs.Length; i++)
                            rib.Add(funArgs[i], args[i]);

                        body = clause.Body;
                        break;
                    }

                    if (clause.Args is VariadicProcArgs) {
                        var variadicArgs = (VariadicProcArgs)clause.Args;
                        if (!SExprUtils.CheckArity(args.Length, variadicArgs.Args.Length, -1)) {
                            if (isLast) {
                                SExprUtils.ReportArityError(
                                    "procedure",
                                    args.Length,
                                    variadicArgs.Args.Length,
                                    -1,
                                    listExpr.Loc);
                            }
                            continue;
                        }

                        for (int i = 0; i < variadicArgs.Args.Length; i++)
                            rib.Add(variadicArgs.Args[i], args[i]);
                        rib.Add(
                            variadicArgs.VariadicArgs,
                            CreateList(args.Skip(variadicArgs.Args.Length).ToArray()));

                        body = clause.Body;
                        break;
                    }

                    throw new SchemeException("Internal error: Unknown clause argument type");
                }

                return new Continuation(
                    body, rib, dynEnv, mode | Mode.AllowDefine, /*isProc=*/true);
            }

            // Handle foreign function calls.
            if (head is NativeProc)
                return ((NativeProc)head).Call(args, env, dynEnv, mode);

            throw new SchemeException("Can't call a value of type " + head.GetType());
        }

        private SchemeObject CreateList(SchemeObject[] values) {
            if (values.Length == 0)
                return SchemeNull.Null;
            return new SchemeList(values[0], values.Skip(1));
        }

        internal string FormatSymbol(string mjsRepr) {
            return mPreferMJSForm ? mjsRepr : Identifier.SwitchRepr(mjsRepr);
        }
    }

    // Dynamic environments

    // The dynamic environment of the currently-executing procedure.
    public class DynamicEnv {
        private DynamicEnv mParent;
        private readonly Dictionary<Parameter, SchemeObject> mParameters;
        public readonly ExceptionHandler ExceptionHandler;

        public DynamicEnv(
                DynamicEnv parent = null,
                ExceptionHandler exceptionHandler = null) {
            mParent = parent;
            mParameters = new Dictionary<Parameter, SchemeObject>();
            ExceptionHandler = exceptionHandler;
        }

        public SchemeObject[] RaiseContinuable(SchemeObject error) {
            DynamicEnv handlerEnv = new DynamicEnv(this, ExceptionHandler.NextHandler);

            SchemeProc handlerProc;
            if (ExceptionHandler == null)
                handlerProc = new DefaultExceptionHandlerProc();
            else
                handlerProc = ExceptionHandler.Proc;

            ListExpr callExpr = new ListExpr(handlerProc, new SExpr[] { error });
            return Interpreter.Current.EvaluateMulti(
                callExpr, new Env(), handlerEnv, Mode.Default, -1);
        }

        public SchemeObject this[Parameter param] {
            get {
                DynamicEnv dynEnv = this;
                while (dynEnv != null) {
                    if (dynEnv.mParameters.ContainsKey(param))
                        return dynEnv.mParameters[param];
                    dynEnv = dynEnv.mParent;
                }
                return null;
            }

            set { mParameters[param] = value; }
        }

        public void OpenDefaultIOPorts() {
            this[IOPorts.Stdin] = new TextualPort(Console.In, null);
            this[IOPorts.Stdout] = new TextualPort(null, Console.Out);
            this[IOPorts.Stderr] = new TextualPort(null, Console.Error);
        }
    }

    public class ExceptionHandler {
        public readonly SchemeProc Proc;
        public readonly ExceptionHandler NextHandler;

        internal ExceptionHandler(SchemeProc proc, ExceptionHandler nextHandler) {
            Proc = proc;
            NextHandler = nextHandler;
        }
    }

    // Name resolution

    public class Env : IEnumerable<KeyValuePair<string, Def>> {
        private Dictionary<string, Def> mDefs;
        private Env mParent;
        private bool mAllowRedefinition;

        internal Env(Env parent = null, bool allowRedefinition = false) {
            mDefs = new Dictionary<string, Def>();
            mParent = parent;
            mAllowRedefinition = allowRedefinition;
        }

        public void Import(ImportSet importSet) {
            foreach (KeyValuePair<string, Def> import in importSet.Imports)
                Add(import.Key, import.Value);
        }

        public void Import(Library library) {
            Import(new LibraryImportSet(library));
        }

        internal Def Get(string name) {
            if (mDefs.ContainsKey(name))
                return mDefs[name];
            if (mParent != null)
                return mParent.Get(name);
            return null;
        }

        public void Add(string name, Def value) {
            if (!mAllowRedefinition && mDefs.ContainsKey(name))
                throw new SchemeException(String.Format("Symbol `{0}` is already defined", name));
            AddOrSet(name, value);
        }

        public void Add(string name, SchemeObject value) {
            Add(name, new ObjectDef(value));
        }

        public void Add(string name, Syntax syntax) {
            Add(name, new SyntaxDef(syntax));
        }

        /// <summary>
        /// Adds a definition to the environment, replacing one that's already there if it exists.
        /// </summary>
        public void AddOrSet(string name, SchemeObject value) {
            AddOrSet(name, new ObjectDef(value));
        }

        /// <summary>
        /// Adds a definition to the environment, replacing one that's already there if it exists.
        /// </summary>
        public void AddOrSet(string name, Def value) {
            mDefs[name] = value;
        }

        internal void Declare(string name) {
            Add(name, new PlaceholderDef());
        }

        internal bool RefersTo(string name, Def query) {
            Def def = Get(name);
            if (def == null)
                return query == null;
            return def.RefersTo(query);
        }

        internal bool RefersTo(SExpr expr, Def def) {
            return expr is Identifier && RefersTo(((Identifier)expr).Name, def);
        }

        internal bool RefersToSyntax<T>(string name) where T: Syntax {
            Def def = Get(name);
            if (!(def is SyntaxDef))
                return false;
            return typeof(T).IsAssignableFrom(((SyntaxDef)def).Value.GetType());
        }

        internal bool RefersToSyntax<T>(SExpr expr) where T : Syntax {
            return expr is Identifier && RefersToSyntax<T>(((Identifier)expr).Name);
        }

        public IEnumerable<string> Symbols {
            get { return mDefs.Keys; }
        }

        public override string ToString() {
            return ToString(new ToStringOptions((ToStringFlags)0));
        }

        internal string ToString(ToStringOptions options) {
            if (options.ContainsFlags(ToStringFlags.MJSExprForm))
                return String.Join(", ", Symbols.ToArray());
            return String.Join(" ", Symbols.ToArray());
        }

        IEnumerator IEnumerable.GetEnumerator() {
            return ((IEnumerable<SchemeObject>)this).GetEnumerator();
        }

        IEnumerator<KeyValuePair<string, Def>>
                IEnumerable<KeyValuePair<string, Def>>.GetEnumerator() {
            return mDefs.GetEnumerator();
        }
    }

    // Definitions for name lookup purposes

    public abstract class Def {
        public abstract bool RefersTo(Def other);
    }

    // A value that hasn't been assigned yet. Evaluating it is an error.
    public class PlaceholderDef : Def {
        public override bool RefersTo(Def other) {
            return this == other;
        }
    }

    public class ObjectDef : Def {
        public SchemeObject Value;

        public ObjectDef(SchemeObject value) {
            Value = value;
        }

        public override bool RefersTo(Def other) {
            return other is ObjectDef && ((ObjectDef)other).Value == Value;
        }
    }

    public class SyntaxDef : Def {
        public readonly Syntax Value;

        public SyntaxDef(Syntax value) {
            Value = value;
        }

        public override bool RefersTo(Def other) {
            return other is SyntaxDef && ((SyntaxDef)other).Value == Value;
        }
    }

    // Tail recursion implementation

    public abstract class Evaluation { }

    public class Result : Evaluation {
        public readonly SchemeObject[] Datums;

        public Result(SchemeObject[] datums) {
            Datums = datums;
        }

        public Result(SchemeObject datum) : this(new SchemeObject[1] { datum }) { }

        public Result() : this(SchemeNull.Null) { }
    }

    public class Continuation : Evaluation {
        public readonly SExpr Expr;
        public readonly Env Env;
        public readonly DynamicEnv DynEnv;
        public readonly Mode Mode;

        /// <summary>
        /// If this is set, then this is the outermost stack frame of a new procedure. This is used
        /// for stack traces.
        /// </summary>
        public readonly bool IsProc;

        public Continuation(
                SExpr expr, Env env, DynamicEnv dynEnv, Mode mode, bool isProc = false) {
            Expr = expr;
            Env = env;
            DynEnv = dynEnv;
            Mode = mode;
            IsProc = isProc;
        }
    }

    // Exception helpers

    internal class DefaultExceptionHandlerProc : SimpleNativeProc {
        protected override SchemeObject Call(SchemeObject[] args) {
            CheckArity(args, 1);
            throw new Exception("Unhandled Scheme exception", new SchemeException(args[0]));
        }
    }

    public class StackFrame {
        public SourceLocation Loc;

        public StackFrame(SourceLocation loc) {
            Loc = loc;
        }

        public override string ToString() {
            return "" + Loc;
        }
    }

    // Built-in syntax

    // 4 Expressions

    // This is almost the same as a Macro, but it can manipulate its environment.
    public abstract class Syntax {
        public abstract Evaluation Evaluate(
            ListExpr listExpr, Env env, DynamicEnv dynEnv, Mode mode);

        internal static SExpr[] GetArguments(ListExpr expr) {
            return expr.Tail;
        }

        protected static void CheckArity(SExpr[] args, int minCount, int maxCount) {
            if (!SExprUtils.CheckArity(args.Length, minCount, maxCount))
                SExprUtils.ReportArityError("syntax", args.Length, minCount, maxCount);
        }

        protected static void CheckArity(SExpr[] args, int count) {
            CheckArity(args, count, count);
        }

        protected static Evaluation EvaluateMultiple(
                IEnumerable<SExpr> exprs,
                Env env,
                DynamicEnv dynEnv,
                Mode mode) {
            if (!exprs.Any())
                return new Result(SchemeNull.Null);

            int index = 0, lastIndex = exprs.Count() - 1;
            foreach (SExpr expr in exprs) {
                if (index < lastIndex) {
                    Interpreter.Current.Evaluate(expr, env, dynEnv, mode);
                    index++;
                    continue;
                }
                return new Continuation(expr, env, dynEnv, mode);
            }

            throw new SchemeException("Unreachable");
        }
    }

    // 4.1.2 Literal expressions

    internal class QuoteForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                Mode mode) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 1);
            return new Result(args[0].Quote());
        }
    }

    // 4.2.7 Exception handling

    public class SchemeException : Exception {
        public readonly SchemeObject Error;

        public SchemeException(SchemeObject error) : base(error.ToString()) {
            Error = error;
        }

        public SchemeException() : this("Error") { }

        public SchemeException(string message) : base(message) {
            Error = new Error(message);
        }

        public SchemeException(string message, Exception inner) : base(message, inner) {
            Error = new Error(message);
        }

        public SchemeException(string message, SourceLocation loc) : base(message) {
            Error = new Error(message, new SchemeObject[] { }, loc);
        }

        public override string ToString() {
            StringBuilder builder = new StringBuilder();
            builder.AppendLine(Message);
            builder.AppendLine("" + Error);

            builder.AppendLine("Native stack trace:");
            builder.AppendLine(StackTrace);

            return "" + builder;
        }

        public void AttachLocation(SourceLocation loc) {
            if (Error != null & Error is Error)
                ((Error)Error).AttachLocation(loc);
        }

        public void PushStackFrame(string name) {
            if (Error != null & Error is Error)
                ((Error)Error).PushStackFrame(name);
        }
    }

    [Flags]
    public enum Mode : byte {
        AllowDefine = 1,
        AllowImports = 2,

        Default = 0,
        Repl = AllowDefine | AllowImports,
    }
}
