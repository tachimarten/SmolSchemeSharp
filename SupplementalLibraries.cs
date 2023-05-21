// SmolScheme/SupplementalLibraries.cs

using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace SmolScheme {
    public static class SmolSchemeExtensionsLibrary {
        public static Library Create() {
            Library library = new Library(new Dictionary<string, Def> {
                { "!=", new ObjectDef(new NumericRelationalProc(RelationalOp.NotEqual)) },
                { "False", new ObjectDef(SchemeBool.False) },
                { "Local", new SyntaxDef(new LocalForm()) },
                { "Null", new ObjectDef(SchemeNull.Null) },
                { "True", new ObjectDef(SchemeBool.True) },
            });
            library.AddBridgedClass(typeof(SmolSchemeExtensionsImpl));
            return library;
        }
    }

    public static class CaseLambdaLibrary {
        public static Library Create() {
            return new Library(new Dictionary<string, Def> {
                { "CaseLambda", new SyntaxDef(new CaseLambdaForm()) },
            });
        }
    }

    public static class CharLibrary {
        public static Library Create() {
            var library = new Library();
            library.AddBridgedClass(typeof(CharLibraryImpl));
            return library;
        }
    }

    public static class ComplexLibrary {
        public static Library Create() {
            var library = new Library();
            library.AddBridgedClass(typeof(ComplexLibraryImpl));
            return library;
        }
    }

    public static class CxrLibrary {
        public static Library Create() {
            var defs = new Dictionary<string, Def>();
            for (int length = 2; length < 5; length++) {
                for (int bitmask = 0; bitmask < (1 << length); bitmask++) {
                    string name = CxrProc.GetName(length, bitmask);
                    defs.Add(name, new ObjectDef(new CxrProc(length, bitmask)));
                }
            }
            return new Library(defs);
        }
    }

    public static class EvalLibrary {
        public static Library Create() {
            var defs = new Dictionary<string, Def>() {
                { "Environment", new ObjectDef(new EnvironmentProc()) },
                { "Eval", new ObjectDef(new EvalProc()) },
            };
            return new Library(defs);
        }
    }

    public static class FileLibrary {
        public static Library Create() {
            var library = new Library();
            library.AddBridgedClass(typeof(FileLibraryImpl));
            return library;
        }
    }

    public static class InexactLibrary {
        public static Library Create() {
            var library = new Library();
            library.AddBridgedClass(typeof(InexactLibraryImpl));
            return library;
        }
    }

    public static class LazyLibrary {
        public static Library Create() {
            var library = new Library(new Dictionary<string, Def> {
                { "Delay", new SyntaxDef(new DelayForm()) },
                { "DelayForce", new SyntaxDef(new DelayForceForm()) },
                { "Force", new ObjectDef(new ForceProc()) },
            });
            library.AddBridgedClass(typeof(LazyLibraryImpl));
            return library;
        }
    }

    public static class LoadLibrary {
        public static Library Create() {
            var library = new Library();
            library.AddBridgedClass(typeof(LoadLibraryImpl));
            return library;
        }
    }

    public static class ProcessContextLibrary {
        public static Library Create() {
            var library = new Library();
            library.AddBridgedClass(typeof(ProcessContextLibraryImpl));
            return library;
        }
    }

    public static class ReadLibrary {
        public static Library Create() {
            var library = new Library();
            library.AddBridgedClass(typeof(ReadLibraryImpl));
            return library;
        }
    }

    public static class ReplLibrary {
        public static Library Create() {
            return new Library(new Dictionary<string, Def> {
                { "InteractionEnvironment", new ObjectDef(new InteractionEnvironmentProc()) },
            });
        }
    }

    public static class TimeLibrary {
        public static Library Create() {
            var library = new Library();
            library.AddBridgedClass(typeof(TimeLibraryImpl));
            return library;
        }
    }

    public static class WriteLibrary {
        public static Library Create() {
            var library = new Library();
            library.AddBridgedClass(typeof(WriteLibraryImpl));
            return library;
        }
    }

    // 4.2.5 Delayed evaluation

    internal static class LazyLibraryImpl {
        [SchemeBridge("Promise?")]
        public static bool IsPromise(SchemeObject obj) {
            return obj is Promise;
        }

        [SchemeBridge]
        public static Promise MakePromise(SchemeObject obj) {
            return new Promise(new Promise.ResolvedPayload(obj));
        }
    }

    internal class DelayForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 1);

            return new Result(new Promise(new Promise.UnresolvedPayload(args[0], env)));
        }
    }

    internal class DelayForceForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 1);

            return new Result(new Promise(new Promise.DelayForcePayload(args[0], env)));
        }
    }

    internal class ForceProc : NativeProc {
        public override Evaluation Call(
                SchemeObject[] args,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            CheckArity(args, 1);
            SchemeObject obj = args[0];

            List<Promise> promisesToResolve = new List<Promise>();
            SchemeObject result;

            while (true) {
                if (!(obj is Promise)) {
                    result = obj;
                    break;
                }

                var promise = (Promise)obj;

                if (promise.Payload is Promise.ResolvedPayload) {
                    result = ((Promise.ResolvedPayload)promise.Payload).Result;
                    break;
                }

                if (promise.Payload is Promise.UnresolvedPayload) {
                    var unresolvedPayload = (Promise.UnresolvedPayload)promise.Payload;
                    result = Interpreter.Current.Evaluate(
                        unresolvedPayload.Expr, unresolvedPayload.Env, dynEnv);
                    promisesToResolve.Add(promise);
                    break;
                }

                // Chain to the next promise.
                var delayForcePayload = (Promise.DelayForcePayload)promise.Payload;
                SchemeObject nextObj = Interpreter.Current.Evaluate(
                    delayForcePayload.Expr, delayForcePayload.Env, dynEnv);
                promisesToResolve.Add(promise);
                obj = nextObj;
            }

            foreach (Promise promise in promisesToResolve)
                promise.Payload = new Promise.ResolvedPayload(result);

            return new Result(result);
        }
    }

    // 4.2.9 Case-lambda

    internal class CaseLambdaForm : Syntax {
        private static string UnpackArg(SExpr arg, int argIndex) {
            if (!(arg is Identifier))
                throw new SchemeException("Procedure argument " + argIndex + " must be a symbol");
            return ((Identifier)arg).Name;
        }

        private static string[] UnpackArgs(IEnumerable<SExpr> argExprs) {
            var args = new List<string>();
            int argIndex = 0;
            foreach (SExpr arg in argExprs) {
                args.Add(UnpackArg(arg, argIndex));
                argIndex++;
            }
            return args.ToArray();
        }

        static internal CaseLambdaClause BuildClause(SExpr sigExpr, SExpr bodyExpr) {
            ProcArgs procArgs;
            if (sigExpr is SchemeNull) {
                procArgs = new NormalProcArgs(new string[0]);
            } else if (sigExpr is ListExpr) {
                procArgs = new NormalProcArgs(UnpackArgs((ListExpr)sigExpr));
            } else if (sigExpr is Identifier) {
                procArgs = new VariadicProcArgs(new string[0], sigExpr.RequireIdentifier());
            } else if (sigExpr is SplatExpr) {
                var splat = (SplatExpr)sigExpr;
                var fixedArgs = UnpackArgs((new SExpr[] { splat.Head }).Concat(splat.Body));
                procArgs = new VariadicProcArgs(fixedArgs, splat.Tail.RequireIdentifier());
            } else {
                throw new SchemeException(
                    "This lambda clause's argument list must be a list, splat, or symbol");
            }

            return new CaseLambdaClause(procArgs, bodyExpr);
        }

        static internal CaseLambdaClause BuildClause(SExpr[] subexprs) {
            if (subexprs.Length != 2)
                throw new SchemeException("This lambda clause must be a list with 2 elements");
            return BuildClause(subexprs[0], subexprs[1]);
        }

        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] clauseExprs = GetArguments(listExpr);
            CaseLambdaClause[] clauses = new CaseLambdaClause[clauseExprs.Length];
            for (int i = 0; i < clauseExprs.Length; i++)
                clauses[i] = BuildClause(clauseExprs[i].RequireListExpr().ToArray());

            return new Result(new SchemeProcDatum(clauses, env));
        }
    }

    // 6.2 Numerics

    // 6.2.6 Numerical operations

    delegate bool MathTestFun(double x);

    internal class MathTestFunProc : TestProc {
        private MathTestFun mFun;

        internal MathTestFunProc(MathTestFun fun) : base() {
            mFun = fun;
        }

        protected override bool Check(SchemeObject arg) {
            return mFun(arg.BridgeTo<double>());
        }
    }

    internal static class InexactLibraryImpl {
        [SchemeBridge]
        public static double Acos(double x) { return Math.Acos(x); }

        [SchemeBridge]
        public static double Asin(double x) { return Math.Asin(x); }

        [SchemeBridge]
        public static double Atan(double x) { return Math.Atan(x); }

        [SchemeBridge]
        public static double Atan(double y, double x) { return Math.Atan2(y, x); }

        [SchemeBridge]
        public static double Cos(double x) { return Math.Cos(x); }

        [SchemeBridge]
        public static double Exp(double x) { return Math.Exp(x); }

        [SchemeBridge("Finite?")]
        public static bool IsFinite(double x) { return !Double.IsInfinity(x); }

        [SchemeBridge("Infinite?")]
        public static bool IsInfinite(double x) { return Double.IsInfinity(x); }

        [SchemeBridge]
        public static double Log(double x) { return Math.Log(x); }

        [SchemeBridge("Nan?")]
        public static bool IsNaN(double x) { return Double.IsNaN(x); }

        [SchemeBridge("Sin")]
        public static double Sin(double x) { return Math.Sin(x); }

        [SchemeBridge("Sqrt")]
        public static double Sqrt(double x) { return Math.Sqrt(x); }

        [SchemeBridge("Tan")]
        public static double Tan(double x) { return Math.Tan(x); }
    }

    internal static class ComplexLibraryImpl {
        [SchemeBridge]
        public static Complex MakeRectangular(double x1, double x2) {
            return new Complex(x1, x2);
        }

        [SchemeBridge]
        public static Complex MakePolar(double x3, double x4) {
            return Complex.FromPolarCoordinates(x3, x4);
        }

        [SchemeBridge]
        public static double RealPart(Complex z) {
            return z.Real;
        }

        [SchemeBridge]
        public static double ImagPart(Complex z) {
            return z.Imaginary;
        }

        [SchemeBridge]
        public static double Magnitude(Complex z) {
            return z.Magnitude;
        }

        [SchemeBridge]
        public static double Angle(Complex z) {
            return z.Phase;
        }
    }

    // 6.4 Pairs and lists

    internal class CxrProc : SimpleNativeProc {
        private int mLength;
        private int mBitmask;

        public CxrProc(int length, int bitmask) {
            mLength = length;
            mBitmask = bitmask;
        }

        public static string GetName(int length, int bitmask) {
            string str = "C";
            for (int i = length - 1; i >= 0; i--)
                str += ((bitmask >> i) & 1) == 0 ? "a" : "d";
            return str + "r";
        }

        protected override SchemeObject Call(SchemeObject[] args) {
            CheckArity(args, 1);
            SchemeObject arg = args[0];

            for (int i = 0; i < mLength; i++) {
                if (!(arg is SchemeList))
                    throw new SchemeException("Argument wasn't a proper list");

                var list = (SchemeList)arg;
                arg = ((mBitmask >> i) & 1) == 0 ? list.Head : list.Tail;
            }

            return arg;
        }
    }

    // 6.6 Characters

    internal static class RelationalOps {
        public static bool Compare<T>(T[] objects, Func<T, T, bool> compare) {
            for (int i = 0; i < objects.Length - 1; i++) {
                if (!compare(objects[i], objects[i + 1]))
                    return false;
            }
            return true;
        }
    }

    internal static class CharLibraryImpl {

        [SchemeBridge("CharAlphabetic?")]
        public static bool IsCharAlphabetic(char c) {
            return Char.IsLetter(c);
        }

        [SchemeBridge("CharCi<=?")]
        public static bool IsCharCiLEqual(params char[] chars) {
            return RelationalOps.Compare(chars, (a, b) => Char.ToLower(a) <= Char.ToLower(b));
        }

        [SchemeBridge("CharCi<?")]
        public static bool IsCharCiLess(params char[] chars) {
            return RelationalOps.Compare(chars, (a, b) => Char.ToLower(a) < Char.ToLower(b));
        }

        [SchemeBridge("CharCi=?")]
        public static bool IsCharCiEqual(params char[] chars) {
            return RelationalOps.Compare(chars, (a, b) => Char.ToLower(a) == Char.ToLower(b));
        }

        [SchemeBridge("CharCi>=?")]
        public static bool IsCharCiGEqual(params char[] chars) {
            return RelationalOps.Compare(chars, (a, b) => Char.ToLower(a) >= Char.ToLower(b));
        }

        [SchemeBridge("CharCi>?")]
        public static bool IsCharCiGreater(params char[] chars) {
            return RelationalOps.Compare(chars, (a, b) => Char.ToLower(a) > Char.ToLower(b));
        }

        [SchemeBridge]
        public static char CharDowncase(char ch) {
            return Char.ToLower(ch);
        }

        /// <summary>
        /// FIXME: Not quite right, see https://github.com/dotnet/corefxlab/issues/2610
        /// </summary>
        [SchemeBridge]
        public static char CharFoldcase(char ch) {
            return Char.ToLower(ch);
        }

        [SchemeBridge("CharLowercase?")]
        public static bool IsCharLower(char ch) {
            return Char.IsLower(ch);
        }

        [SchemeBridge("CharNumeric?")]
        public static bool IsCharNumeric(char ch) {
            return Char.IsNumber(ch);
        }

        [SchemeBridge]
        public static char CharUpcase(char ch) {
            return Char.ToUpper(ch);
        }

        [SchemeBridge("CharUppercase?")]
        public static bool IsUpper(char ch) {
            return Char.IsUpper(ch);
        }

        [SchemeBridge]
        public static SchemeObject DigitValue(char ch) {
            double value = Char.GetNumericValue(ch);
            if (value >= 0.0)
                return SchemeNumber.Create(value);
            return SchemeBool.False;
        }

        [SchemeBridge("StringCi<=?")]
        public static bool IsStringCiLEqual(params string[] strings) {
            return RelationalOps.Compare(strings,
                (a, b) => a.ToLower().CompareTo(b.ToLower()) <= 0);
        }

        [SchemeBridge("StringCi<?")]
        public static bool IsStringCiLess(params string[] strings) {
            return RelationalOps.Compare(strings,
                (a, b) => a.ToLower().CompareTo(b.ToLower()) < 0);
        }

        [SchemeBridge("StringCi=?")]
        public static bool IsStringCiEqual(params string[] strings) {
            return RelationalOps.Compare(strings,
                (a, b) => a.ToLower().CompareTo(b.ToLower()) == 0);
        }

        [SchemeBridge("StringCi>=?")]
        public static bool IsStringCiGEqual(params string[] strings) {
            return RelationalOps.Compare(strings,
                (a, b) => a.ToLower().CompareTo(b.ToLower()) >= 0);
        }

        [SchemeBridge("StringCi>?")]
        public static bool IsStringCiGreater(params string[] strings) {
            return RelationalOps.Compare(strings,
                (a, b) => a.ToLower().CompareTo(b.ToLower()) > 0);
        }

        [SchemeBridge]
        public static string StringDowncase(string str) {
            return str.ToLower();
        }

        [SchemeBridge]
        public static string StringUpcase(string str) {
            return str.ToUpper();
        }
    }

    // 6.12 Environments and evaluation

    internal class EnvironmentProc : NativeProc {
        public override Evaluation Call(
                SchemeObject[] args,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            Env newEnv = new Env();
            foreach (SchemeObject arg in args)
                newEnv.Import(Interpreter.Current.Libraries.CreateImportSet(arg.Unquote()));
            return new Result(new EnvObject(newEnv));
        }
    }

    internal class InteractionEnvironmentProc : NativeProc {
        public override Evaluation Call(
                SchemeObject[] args,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            Env interactionEnv = Interpreter.Current.Libraries.CreateInteractionEnvironment();
            return new Result(new EnvObject(interactionEnv));
        }
    }

    internal class SchemeReportEnvironmentProc : NativeProc {
        public override Evaluation Call(
                SchemeObject[] args,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            CheckArity(args, 1);
            if (args[0].RequireExactInt() != 5) {
                throw new SchemeException(
                    "Only revision 5 is supported by SchemeReportEnvironment[]");
            }

            Env r5rsEnv = Interpreter.Current.Libraries.CreateR5RSEnvironment();
            return new Result(new EnvObject(r5rsEnv));
        }
    }

    internal class NullEnvironmentProc : NativeProc {
        public override Evaluation Call(
                SchemeObject[] args,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            CheckArity(args, 1);
            if (args[0].RequireExactInt() != 5)
                throw new SchemeException("Only revision 5 is supported by NullEnvironment[]");

            Env nullEnv = Interpreter.Current.Libraries.CreateNullR5RSEnvironment();
            return new Result(new EnvObject(nullEnv));
        }
    }

    internal class EvalProc : NativeProc {
        public override Evaluation Call(
                SchemeObject[] args,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            CheckArity(args, 2);
            SExpr exprOrDef = args[0].Unquote();
            Env subenv = args[1].Require<EnvObject>().Env;

            return new Continuation(exprOrDef, subenv, dynEnv, true);
        }
    }

    // 6.13 Input and output

    // 6.13.2 Input

    internal static class ReadLibraryImpl {
        [SchemeBridge]
        public static SchemeObject Read(TextualPort port) {
            if (port.Reader == null)
                throw new SchemeException("Port isn't open for reading");
            return new SReader(port.Reader).ParseSExpr().Quote();
        }
    }

    // 6.13.3 Output

    internal static class FileLibraryImpl {
        [SchemeBridge("FileExists?")]
        public static bool FileExists(string path) {
            return File.Exists(path);
        }

        [SchemeBridge]
        public static void DeleteFile(string path) {
            try {
                if (!File.Exists(path))
                    throw new SchemeException(new FileError("File doesn't exist"));
                File.Delete(path);
            } catch (Exception e) {
                throw new SchemeException(new FileError("File deletion failed: " + e));
            }
        }

        [SchemeBridge]
        public static BinaryPort OpenBinaryInputFile(string path) {
            return new BinaryPort(new FileStream(path, FileMode.Open));
        }

        [SchemeBridge]
        public static BinaryPort OpenBinaryOutputFile(string path) {
            return new BinaryPort(new FileStream(path, FileMode.Create));
        }

        [SchemeBridge]
        public static StreamTextualPort OpenInputFile(string path) {
            return new StreamTextualPort(new FileStream(path, FileMode.Open));
        }

        [SchemeBridge]
        public static StreamTextualPort OpenOutputFile(string path) {
            return new StreamTextualPort(new FileStream(path, FileMode.Create));
        }
    }

    // 6.14 System interface

    internal static class ProcessContextLibraryImpl {
        [SchemeBridge]
        public static string[] CommandLine() {
            return Environment.GetCommandLineArgs();
        }

        private static int ConvertExitStatus(SchemeObject obj) {
            if (obj is SchemeNumber) {
                SchemeNumber number = (SchemeNumber)obj;
                int exitStatus;
                if (number.TryBridgeTo<int>(out exitStatus))
                    return exitStatus;
            }

            if (obj is SchemeBool) {
                return ((SchemeBool)obj).Value ? 0 : 1;
            }

            return 0;
        }

        [SchemeBridge]
        public static void Exit(SchemeObject obj = null) {
            Environment.Exit(ConvertExitStatus(obj));
        }

        [SchemeBridge]
        public static void EmergencyExit(SchemeObject obj = null) {
            // FIXME: This should return an exit code. But I don't think the .NET platform has a
            // function that allows for this.
            Environment.FailFast("" + ConvertExitStatus(obj));
        }

        [SchemeBridge]
        public static SchemeObject GetEnvironmentVariable(string varName) {
            string value = Environment.GetEnvironmentVariable(varName);
            if (value == null)
                return SchemeBool.False;
            return (SchemeObject)new SchemeString(value);
        }

        [SchemeBridge]
        public static IDictionary GetEnvironmentVariables() {
            return Environment.GetEnvironmentVariables();
        }
    }

    internal static class LoadLibraryImpl {
        [SchemeBridge]
        public static void Load(string path) {
            Interpreter.Current.Load(new Env(), SimpleNativeProc.CurrentDynamicEnv, path);
        }
    }

    internal static class TimeLibraryImpl {
        internal static readonly DateTime SchemeEpoch =
            new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).AddSeconds(-8.000082);

        [SchemeBridge]
        public static double CurrentSecond() {
            return (DateTime.UtcNow - SchemeEpoch).TotalSeconds;
        }

        [SchemeBridge]
        public static long CurrentJiffy() {
            // FIXME: May lose precision.
            return (long)(DateTime.UtcNow - SchemeEpoch).TotalMilliseconds;
        }

        [SchemeBridge]
        public static int JiffiesPerSecond() {
            return 1000;
        }
    }

    internal static class WriteLibraryImpl {

        public static void Write(
                SchemeObject obj, TextualPort port, ToStringFlags flags, bool checkForCycles) {
            if (port == null)
                port = (TextualPort)IOPorts.Stdout.Get();

            SchemeObjectPtrSet cycles = null;
            if (checkForCycles) {
                cycles = new SchemeObjectPtrSet();
                obj.FindCycles(new SchemeObjectPtrSet(), cycles);
            }

            port.Write(obj.ToString(new ToStringOptions(flags, cycles)));
        }

        [SchemeBridge]
        public static void Write(SchemeObject obj, TextualPort port = null) {
            Write(obj, port, (ToStringFlags)0, /*checkForCycles=*/true);
        }

        [SchemeBridge]
        public static void WriteShared(SchemeObject obj, TextualPort port = null) {
            Write(obj, port, (ToStringFlags)0, /*checkForCycles=*/true);
        }

        [SchemeBridge]
        public static void WriteSimple(SchemeObject obj, TextualPort port = null) {
            Write(obj, port, (ToStringFlags)0, /*checkForCycles=*/false);
        }

        [SchemeBridge]
        public static void Display(SchemeObject obj, TextualPort port = null) {

            Write(obj, port, ToStringFlags.RawStringsAndChars, /*checkForCycles=*/true);
        }
    }

    // Extensions

    internal class LocalForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 1);
            return new Continuation(args[0], new Env(env), dynEnv, true);
        }
    }

    internal static class SmolSchemeExtensionsImpl {
        [SchemeBridge]
        public static readonly SchemeNumber Pi = new SchemeComplex((Complex)Math.PI);

        [SchemeBridge]
        public static void RequireAssembly(params string[] assemblyPaths) {
            foreach (string assemblyPath in assemblyPaths)
                Assembly.Load(assemblyPath);
        }

        [SchemeBridge]
        public static SchemeObject First(SchemeList list) {
            return list.Head;
        }

        [SchemeBridge]
        public static SchemeObject Second(SchemeList list) {
            return list[1];
        }

        [SchemeBridge]
        public static SchemeObject Rest(SchemeList list) {
            return list.Tail;
        }
    }
}
