// SmolScheme/BaseLibrary.cs

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Text;

namespace SmolScheme {
    public static class BaseLibrary {
        public static Library Create() {
            CallCCProc callCC = new CallCCProc();
            Library library = new Library(new Dictionary<string, Def> {
                { "...", SyntaxRulesMacro.Ellipsis },
                { "<", new ObjectDef(new NumericRelationalProc(RelationalOp.Less)) },
                { "<=", new ObjectDef(new NumericRelationalProc(RelationalOp.LEqual)) },
                { "=", new ObjectDef(new NumericRelationalProc(RelationalOp.Equal)) },
                { "=>", CondForm.Arrow },
                { ">", new ObjectDef(new NumericRelationalProc(RelationalOp.Greater)) },
                { ">=", new ObjectDef(new NumericRelationalProc(RelationalOp.GEqual)) },
                { "_", SyntaxRulesMacro.Underscore },
                { "And", new SyntaxDef(new AndForm()) },
                { "Apply", new ObjectDef(new ApplyProc()) },
                { "Begin", new SyntaxDef(new BeginForm()) },
                { "Call/cc", new ObjectDef(callCC) },
                { "CallWithCurrentContinuation", new ObjectDef(callCC) },
                { "CallWithInputFile", new ObjectDef(new CallWithInputFileProc()) },
                { "CallWithOutputFile", new ObjectDef(new CallWithOutputFileProc()) },
                { "CallWithPort", new ObjectDef(new CallWithPortProc()) },
                { "CallWithValues", new ObjectDef(new CallWithValuesProc()) },
                { "Case", new SyntaxDef(new CaseForm()) },
                { "Char<=?", new ObjectDef(new CharRelationalProc(RelationalOp.LEqual)) },
                { "Char<?", new ObjectDef(new CharRelationalProc(RelationalOp.Less)) },
                { "Char>=?", new ObjectDef(new CharRelationalProc(RelationalOp.GEqual)) },
                { "Char>?", new ObjectDef(new CharRelationalProc(RelationalOp.Greater)) },
                { "Cond", new SyntaxDef(new CondForm()) },
                { "CondExpand", new SyntaxDef(new CondExpandForm()) },
                { "CurrentErrorPort", new ObjectDef(IOPorts.Stderr) },
                { "CurrentInputPort", new ObjectDef(IOPorts.Stdin) },
                { "CurrentOutputPort", new ObjectDef(IOPorts.Stdout) },
                { "Define", new SyntaxDef(new DefineForm()) },
                { "DefineLibrary", new SyntaxDef(new DefineLibraryForm()) },
                { "DefineRecordType", new SyntaxDef(new DefineRecordTypeForm()) },
                { "DefineSyntax", new SyntaxDef(new DefineSyntaxForm()) },
                { "DefineValues", new SyntaxDef(new DefineValuesForm()) },
                { "Do", new SyntaxDef(new DoForm()) },
                { "Else", CondForm.Else },
                { "Guard", new SyntaxDef(new GuardForm()) },
                { "If", new SyntaxDef(new IfMacro()) },
                // FIXME: This needs to be at the beginning only.
                { "Import", new SyntaxDef(new ImportForm()) },
                { "Include", new SyntaxDef(new IncludeForm(/*caseInsensitive=*/false)) },
                { "IncludeCi", new SyntaxDef(new IncludeForm(/*caseInsensitive=*/true)) },
                { "Lambda", new SyntaxDef(new LambdaForm()) },
                { "Let", new SyntaxDef(new LetForm(/*isRecursive=*/false, /*isStar=*/false)) },
                { "Let*", new SyntaxDef(new LetForm(/*isRecursive=*/false, /*isStar=*/true)) },
                { "Let*Values", new SyntaxDef(new LetValuesForm(/*isStar=*/true)) },
                { "LetSyntax", new SyntaxDef(new LetSyntaxForm(/*isRecursive=*/false)) },
                { "LetValues", new SyntaxDef(new LetValuesForm(/*isStar=*/false)) },
                { "Letrec", new SyntaxDef(new LetForm(/*isRecursive=*/true, /*isStar=*/false)) },
                { "Letrec*", new SyntaxDef(new LetForm(/*isRecursive=*/true, /*isStar=*/true)) },
                { "LetrecSyntax", new SyntaxDef(new LetSyntaxForm(/*isRecursive=*/true)) },
                { "Not", new ObjectDef(new NotProc()) },
                { "Or", new SyntaxDef(new OrForm()) },
                { "Parameterize", new SyntaxDef(new ParameterizeForm()) },
                { "Quasiquote", new SyntaxDef(new QuasiquoteForm()) },
                { "Quote", new SyntaxDef(new QuoteForm()) },
                { "Raise", new ObjectDef(new RaiseProc()) },
                { "RaiseContinuable", new ObjectDef(new RaiseContinuableProc()) },
                { "Set!", new SyntaxDef(new SetForm()) },
                { "String<=?", new ObjectDef(new StringRelationalProc(RelationalOp.LEqual)) },
                { "String<?", new ObjectDef(new StringRelationalProc(RelationalOp.Less)) },
                { "String=?", new ObjectDef(new StringRelationalProc(RelationalOp.Equal)) },
                { "String>=?", new ObjectDef(new StringRelationalProc(RelationalOp.GEqual)) },
                { "String>?", new ObjectDef(new StringRelationalProc(RelationalOp.Greater)) },
                { "SyntaxError", new SyntaxDef(new SyntaxErrorForm()) },
                { "Unless", new SyntaxDef(new UnlessMacro()) },
                { "Unquote", QuasiquoteForm.Unquote },
                { "UnquoteSplicing", QuasiquoteForm.UnquoteSplicing },
                { "Values", new ObjectDef(new ValuesProc()) },
                { "When", new SyntaxDef(new WhenMacro()) },
                { "WithExceptionHandler", new ObjectDef(new WithExceptionHandlerProc()) },
                { "WithInputFromFile", new ObjectDef(new WithInputFromFileProc()) },
                { "WithOutputToFile", new ObjectDef(new WithOutputToFileProc()) },
            });
            library.AddBridgedClass(typeof(BaseLibraryImpl));
            library.AddBridgedClass(typeof(BinaryPort));
            library.AddBridgedClass(typeof(Bytevector));
            library.AddBridgedClass(typeof(BytevectorPort));
            library.AddBridgedClass(typeof(EofObject));
            library.AddBridgedClass(typeof(Error));
            library.AddBridgedClass(typeof(FileError));
            library.AddBridgedClass(typeof(Parameter));
            library.AddBridgedClass(typeof(Port));
            library.AddBridgedClass(typeof(ReadError));
            library.AddBridgedClass(typeof(SchemeBool));
            library.AddBridgedClass(typeof(SchemeChar));
            library.AddBridgedClass(typeof(SchemeList));
            library.AddBridgedClass(typeof(SchemeNull));
            library.AddBridgedClass(typeof(SchemeNumber));
            library.AddBridgedClass(typeof(SchemeObject));
            library.AddBridgedClass(typeof(SchemeProc));
            library.AddBridgedClass(typeof(SchemeProcDatum));
            library.AddBridgedClass(typeof(SchemeString));
            library.AddBridgedClass(typeof(StringInputTextualPort));
            library.AddBridgedClass(typeof(StringOutputTextualPort));
            library.AddBridgedClass(typeof(Symbol));
            library.AddBridgedClass(typeof(TextualPort));
            library.AddBridgedClass(typeof(Vector));
            return library;
        }
    }

    // Helpers

    public abstract class TestProc : SimpleNativeProc {
        protected abstract bool Check(SchemeObject arg);

        protected sealed override SchemeObject Call(SchemeObject[] args) {
            CheckArity(args, 1);
            return SchemeBool.Create(Check(args[0]));
        }
    }

    // Logic

    internal class NotProc : SimpleNativeProc {
        protected override SchemeObject Call(SchemeObject[] args) {
            CheckArity(args, 1);
            return SchemeBool.Create(!RequireBool(args[0]));
        }
    }

    // 5.5 Record-type definitions

    internal class MakeRecordProc : SimpleNativeProc {
        private RecordType mRecordType;

        public MakeRecordProc(RecordType recordType) {
            this.mRecordType = recordType;
        }

        protected override SchemeObject Call(SchemeObject[] args) {
            CheckArity(args, mRecordType.FieldCount);
            return new Record(mRecordType, args);
        }
    }

    internal class IsRecordProc : TestProc {
        private RecordType mRecordType;

        public IsRecordProc(RecordType recordType) {
            this.mRecordType = recordType;
        }

        protected override bool Check(SchemeObject arg) {
            return arg is Record && ((Record)arg).Type == mRecordType;
        }
    }

    internal class RecordGetProc : SimpleNativeProc {
        private RecordType mRecordType;
        private int mFieldIndex;

        public RecordGetProc(RecordType recordType, int fieldIndex) {
            mRecordType = recordType;
            mFieldIndex = fieldIndex;
        }

        protected override SchemeObject Call(SchemeObject[] args) {
            CheckArity(args, 1);
            return args[0].RequireRecord(mRecordType).Fields[mFieldIndex];
        }
    }

    internal class RecordSetProc : SimpleNativeProc {
        private RecordType mRecordType;
        private int mFieldIndex;

        public RecordSetProc(RecordType recordType, int fieldIndex) {
            mRecordType = recordType;
            mFieldIndex = fieldIndex;
        }

        protected override SchemeObject Call(SchemeObject[] args) {
            CheckArity(args, 2);
            args[0].RequireRecord(mRecordType).Fields[mFieldIndex] = args[1];
            return SchemeNull.Null;
        }
    }

    // 6.2 Numerics

    // 6.2.6 Numerical operations

    internal abstract class RelationalProc : SimpleNativeProc {
        protected readonly RelationalOp mOp;

        public RelationalProc(RelationalOp op) {
            mOp = op;
        }

        protected abstract bool Check(SchemeObject lhs, SchemeObject rhs);

        protected sealed override SchemeObject Call(SchemeObject[] args) {
            CheckArity(args, 2, -1);

            for (int i = 0; i < args.Length - 1; i++) {
                if (!Check(args[i], args[i + 1]))
                    return SchemeBool.False;
            }

            return SchemeBool.True;
        }
    }

    internal class NumericRelationalProc : RelationalProc {
        public NumericRelationalProc(RelationalOp op) : base(op) { }

        protected override bool Check(SchemeObject lhsObj, SchemeObject rhsObj) {
            SchemeNumber lhs = (SchemeNumber)lhsObj, rhs = (SchemeNumber)rhsObj;

            switch (mOp) {
                case RelationalOp.Equal:
                    return lhs.Eqv(rhs);
                case RelationalOp.NotEqual:
                    return lhs.NotEqual(rhs);
                case RelationalOp.Less:
                    return lhs.LessThan(rhs);
                case RelationalOp.LEqual:
                    return lhs.LessThanEqual(rhs);
                case RelationalOp.Greater:
                    return lhs.GreaterThan(rhs);
                case RelationalOp.GEqual:
                    return lhs.GreaterThanEqual(rhs);
                default:
                    throw new Exception("Unreachable");
            }
        }
    }

    internal abstract class ComparableRelationalProc : NumericRelationalProc {
        public ComparableRelationalProc(RelationalOp op) : base(op) { }

        protected abstract IComparable Require(SchemeObject datum);

        protected sealed override bool Check(SchemeObject lhSchemeObject, SchemeObject rhSchemeObject) {
            int ordering = Require(lhSchemeObject).CompareTo(Require(rhSchemeObject));
            switch (mOp) {
                case RelationalOp.Equal:
                    return ordering == 0;
                case RelationalOp.NotEqual:
                    return ordering != 0;
                case RelationalOp.Less:
                    return ordering < 0;
                case RelationalOp.LEqual:
                    return ordering <= 0;
                case RelationalOp.Greater:
                    return ordering > 0;
                case RelationalOp.GEqual:
                    return ordering >= 0;
                default:
                    throw new Exception("Unreachable");
            }
        }
    }

    public enum RelationalOp {
        Equal,
        NotEqual,
        Less,
        LEqual,
        Greater,
        GEqual,
    }

    // 6.6 Characters

    internal class CharRelationalProc : ComparableRelationalProc {
        public CharRelationalProc(RelationalOp op) : base(op) { }

        protected override IComparable Require(SchemeObject datum) {
            return datum.BridgeTo<char>();
        }
    }

    // 6.7 Strings

    internal class StringRelationalProc : ComparableRelationalProc {
        public StringRelationalProc(RelationalOp op) : base(op) { }

        protected override IComparable Require(SchemeObject datum) {
            return datum.BridgeTo<string>();
        }
    }

    // 6.10 Control features

    internal class ApplyProc : NativeProc {
        public override Evaluation Call(
                SchemeObject[] args, Env env, DynamicEnv dynEnv, bool allowDefine) {
            CheckArity(args, 2, -1);

            var callArgs = new List<SchemeObject>();
            foreach (SchemeObject list in args.Skip(1)) {
                if (list is SchemeNull)
                    continue;
                if (!(list is SchemeList))
                    throw new SchemeException("Arguments to Apply must be lists");
                callArgs.AddRange((SchemeList)list);
            }

            var callExpr = new ListExpr(args[0], callArgs.ToArray());
            return new Continuation(callExpr, env, dynEnv, allowDefine);
        }
    }

    // NB: This implements delimited one-shot continuations, because .NET doesn't support full
    // continuations.
    internal class CallCCProc : NativeProc {
        public override Evaluation Call(
                SchemeObject[] args,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            CheckArity(args, 1);
            SchemeProc proc = args[0].Require<SchemeProc>();

            try {
                ListExpr callExpr = new ListExpr(proc, new SExpr[] { new CallCCEscapeProc() });
                return new Result(Interpreter.Current.EvaluateMulti(callExpr, env, dynEnv));
            } catch (CallCCException callCCException) {
                return new Result(callCCException.Args);
            }
        }

        private class CallCCEscapeProc : SimpleNativeProc {
            protected override SchemeObject Call(SchemeObject[] args) {
                throw new CallCCException(args);
            }
        }

        private class CallCCException : Exception {
            public SchemeObject[] Args;

            public CallCCException(SchemeObject[] args) : base("(call/cc)") {
                Args = args;
            }
        }
    }

    internal class ValuesProc : NativeProc {
        public override Evaluation Call(
                SchemeObject[] args,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            CheckArity(args, 1, -1);
            return new Result(args);
        }
    }

    internal class CallWithValuesProc : NativeProc {
        public override Evaluation Call(
                SchemeObject[] args, Env env, DynamicEnv dynEnv, bool allowDefine) {
            CheckArity(args, 2);
            SchemeProc producer = args[0].Require<SchemeProc>();
            SchemeProc consumer = args[1].Require<SchemeProc>();

            ListExpr callExpr = new ListExpr(args[0], new SExpr[0]);
            SchemeObject[] datums = Interpreter.Current.EvaluateMulti(callExpr, env, dynEnv);
            return new Continuation(new ListExpr(args[1], datums), env, dynEnv, false);
        }
    }

    // 6.11 Exceptions

    internal class WithExceptionHandlerProc : NativeProc {
        public override Evaluation Call(
                SchemeObject[] args,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            CheckArity(args, 2);
            SchemeProc handler = args[0].Require<SchemeProc>();
            SchemeProc thunk = args[1].Require<SchemeProc>();

            DynamicEnv dynSubenv = new DynamicEnv(
                dynEnv,
                new ExceptionHandler(handler, dynEnv.ExceptionHandler));

            ListExpr callExpr = new ListExpr(thunk, new SExpr[0]);
            try {
                return new Result(Interpreter.Current.EvaluateMulti(callExpr, env, dynSubenv));
            } catch (SchemeException exception) {
                dynSubenv.RaiseContinuable(exception.Error);
                throw exception;
            }
        }
    }

    internal class RaiseProc : NativeProc {
        public override Evaluation Call(
                SchemeObject[] args,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            CheckArity(args, 1);
            throw new SchemeException(args[0]);
        }
    }

    internal class RaiseContinuableProc : NativeProc {
        public override Evaluation Call(
                SchemeObject[] args,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            CheckArity(args, 1);
            return new Result(dynEnv.RaiseContinuable(args[0]));
        }
    }

    /// <summary>
    /// Per § 6.13.1, these functions have to yield multiple values, so it can't be bridged right
    /// now.
    /// </summary>
    public abstract class AutoCloseProc : NativeProc {
        protected abstract Port CreatePort(SchemeObject arg);

        protected virtual SExpr[] GetThunkArgs(Port port) {
            return new SExpr[1] { port };
        }

        protected virtual DynamicEnv GetDynamicEnv(DynamicEnv parentDynEnv, Port port) {
            return parentDynEnv;
        }

        public sealed override Evaluation Call(
                SchemeObject[] args, Env env, DynamicEnv dynEnv, bool allowDefine) {
            CheckArity(args, 2);
            Port port = CreatePort(args[0]);
            SchemeObject thunk = Interpreter.Current.Evaluate(args[1], env, dynEnv);

            try {
                SchemeObject[] results = Interpreter.Current.EvaluateMulti(new ListExpr(
                    thunk, GetThunkArgs(port)), env, GetDynamicEnv(dynEnv, port));
                return new Result(results);
            } finally {
                port.Close();
            }
        }
    }

    public sealed class CallWithPortProc : AutoCloseProc {
        protected override Port CreatePort(SchemeObject arg) {
            return arg.Require<Port>();
        }
    }

    public sealed class CallWithInputFileProc : AutoCloseProc {
        protected override Port CreatePort(SchemeObject arg) {
            return FileLibraryImpl.OpenInputFile(arg.BridgeTo<string>());
        }
    }

    public sealed class CallWithOutputFileProc : AutoCloseProc {
        protected override Port CreatePort(SchemeObject arg) {
            return FileLibraryImpl.OpenOutputFile(arg.BridgeTo<string>());
        }
    }

    public sealed class WithInputFromFileProc : AutoCloseProc {
        protected override Port CreatePort(SchemeObject arg) {
            return FileLibraryImpl.OpenInputFile(arg.BridgeTo<string>());
        }

        protected override SExpr[] GetThunkArgs(Port port) {
            return new SExpr[0];
        }

        protected override DynamicEnv GetDynamicEnv(DynamicEnv parentDynEnv, Port port) {
            DynamicEnv dynSubenv = new DynamicEnv(parentDynEnv);
            dynSubenv[IOPorts.Stdin] = port;
            return dynSubenv;
        }
    }

    public sealed class WithOutputToFileProc : AutoCloseProc {
        protected override Port CreatePort(SchemeObject arg) {
            return FileLibraryImpl.OpenOutputFile(arg.BridgeTo<string>());
        }

        protected override SExpr[] GetThunkArgs(Port port) {
            return new SExpr[0];
        }

        protected override DynamicEnv GetDynamicEnv(DynamicEnv parentDynEnv, Port port) {
            DynamicEnv dynSubenv = new DynamicEnv(parentDynEnv);
            dynSubenv[IOPorts.Stdout] = port;
            return dynSubenv;
        }
    }

    /// <summary>
    /// Parameters representing the standard Scheme I/O ports.
    /// </summary>
    public static class IOPorts {
        public static readonly Parameter Stdin = new Parameter();
        public static readonly Parameter Stdout = new Parameter();
        public static readonly Parameter Stderr = new Parameter();
    }

    // Bridged procedures

    internal static class BaseLibraryImpl {
        // 6.2.7 Numerical input and output

        [SchemeBridge("Number->String")]
        public static string NumberToString(SchemeNumber number, int radix = 10) {
            switch (radix) {
                case 2:
                case 8:
                case 10:
                case 16:
                    break;
                default:
                    throw new SchemeException("Radix argument must be 2, 8, 10, or 16");
            }

            int value;
            if (number.TryBridgeTo<int>(out value))
                return Convert.ToString(value, radix);
            return "" + number;
        }

        [SchemeBridge("String->Number")]
        public static SchemeObject StringToNumber(string str, int radix = 10) {
            if (str.Length == 0)
                return SchemeBool.False;

            bool isPrefixed = str[0] == '#';
            if (isPrefixed)
                str = str.Substring(1);

            NumberToken token;
            try {
                SLexer lexer = new SLexer(new StringReader(str));
                if (isPrefixed)
                    token = lexer.ParsePrefixedNumber();
                else
                    token = lexer.ParseNumber(radix, null);
            } catch (ParseException) {
                return SchemeBool.False;
            }

            if (token == null)
                return SchemeBool.False;

            return SchemeNumber.Create(token);
        }

        // 6.10 Control features

        [SchemeBridge]
        public static SchemeObject DynamicWind(
                Action before, Func<SchemeObject> thunk, Action after) {
            before();
            try {
                return thunk();
            } finally {
                after();
            }
        }

        // 6.13 Input and output

        // 6.13.1 Ports

        [SchemeBridge("InputPort?")]
        public static bool IsInputPort(SchemeObject arg) {
            return arg is Port && ((Port)arg).IsInput;
        }

        [SchemeBridge("OutputPort?")]
        public static bool IsOutputPort(SchemeObject arg) {
            return arg is Port && ((Port)arg).IsOutput;
        }

        // 6.13.2 Input

        [SchemeBridge]
        public static SchemeObject ReadChar(TextualPort port = null) {
            int ch = (port == null ? (TextualPort)IOPorts.Stdin.Get() : port).Read();
            return ch < 0 ? (SchemeObject)SmolScheme.EofObject.Eof : new SchemeChar((char)ch);
        }

        [SchemeBridge]
        public static SchemeObject PeekChar(TextualPort port = null) {
            int ch = (port == null ? (TextualPort)IOPorts.Stdin.Get() : port).Peek();
            return ch < 0 ? (SchemeObject)SmolScheme.EofObject.Eof : new SchemeChar((char)ch);
        }

        [SchemeBridge]
        public static SchemeObject ReadLine(TextualPort port = null) {
            string str = (port == null ? (TextualPort)IOPorts.Stdin.Get() : port).ReadLine();
            return str == null ? (SchemeObject)SmolScheme.EofObject.Eof : new SchemeString(str);
        }

        [SchemeBridge]
        public static SmolScheme.EofObject EofObject() {
            return SmolScheme.EofObject.Eof;
        }

        [SchemeBridge]
        public static SchemeObject ReadString(int charCount, TextualPort port = null) {
            if (port == null)
                port = (TextualPort)IOPorts.Stdin.Get();
            char[] buffer = new char[charCount];

            int nRead = port.ReadBlock(buffer, 0, charCount);
            if (nRead == 0)
                return SmolScheme.EofObject.Eof;
            return new SchemeString(new String(buffer, 0, nRead));
        }

        [SchemeBridge]
        public static SchemeObject ReadU8(BinaryPort port = null) {
            int value = (port == null ? (BinaryPort)IOPorts.Stdin.Get() : port).ReadByte();
            if (value < 0)
                return SmolScheme.EofObject.Eof;
            return SchemeNumber.Create(value);
        }

        [SchemeBridge]
        public static SchemeObject PeekU8(BinaryPort port = null) {
            int value = (port == null ? (BinaryPort)IOPorts.Stdin.Get() : port).Peek();
            if (value < 0)
                return SmolScheme.EofObject.Eof;
            return SchemeNumber.Create(value);
        }

        [SchemeBridge("U8Ready?")]
        public static bool U8Ready(BinaryPort port = null) {
            return (port == null ? (BinaryPort)IOPorts.Stdin.Get() : port).Peek() >= 0;
        }

        [SchemeBridge("ReadBytevector")]
        public static Bytevector ReadBytevector(int count, BinaryPort port = null) {
            byte[] buffer = new byte[count];
            int nRead = (port == null ? (BinaryPort)IOPorts.Stdin.Get() : port).ReadAll(buffer);

            // FIXME: Kind of inefficient.
            if (buffer.Length != nRead)
                Array.Resize(ref buffer, nRead);

            return new Bytevector(buffer);
        }

        [SchemeBridge("ReadBytevector!")]
        public static SchemeObject ReadBytevector(
                Bytevector bytevector, BinaryPort port, int start, int end) {
            if (port == null)
                port = (BinaryPort)IOPorts.Stdin.Get();

            int nRead = port.ReadAll(bytevector.Elements, start, end - start);
            if (nRead == 0)
                return SmolScheme.EofObject.Eof;
            return SchemeNumber.Create(nRead);
        }

        [SchemeBridge("ReadBytevector!")]
        public static SchemeObject ReadBytevector(
                Bytevector bytevector, BinaryPort port = null, int start = 0) {
            return ReadBytevector(bytevector, port, start, bytevector.Length);
        }

        // 6.13.3 Output

        [SchemeBridge]
        public static void Newline(TextualPort port = null) {
            (port == null ? (TextualPort)IOPorts.Stdout.Get() : port).WriteLine();
        }

        [SchemeBridge]
        public static void WriteChar(char ch, TextualPort port = null) {
            (port == null ? (TextualPort)IOPorts.Stdout.Get() : port).Write(ch);
        }

        [SchemeBridge]
        public static void WriteString(string str, TextualPort port, int start, int end) {
            if (port == null)
                port = (TextualPort)IOPorts.Stdout.Get();
            port.Write(str.Substring(start, end - start));
        }

        [SchemeBridge]
        public static void WriteString(string str, TextualPort port = null, int start = 0) {
            WriteString(str, port, 0, str.Length);
        }

        [SchemeBridge]
        public static void WriteU8(byte b, BinaryPort port = null) {
            (port == null ? (BinaryPort)IOPorts.Stdout.Get() : port).WriteByte(b);
        }

        [SchemeBridge]
        public static void WriteBytevector(
                Bytevector bytevector, BinaryPort port, int start, int end) {
            if (port == null)
                port = (BinaryPort)IOPorts.Stdout.Get();
            port.Write(bytevector.Elements, start, end - start);
        }

        [SchemeBridge]
        public static void WriteBytevector(
                Bytevector bytevector, BinaryPort port = null, int start = 0) {
            WriteBytevector(bytevector, port, start, bytevector.Length);
        }

        [SchemeBridge]
        public static void FlushOutputPort(Port port = null) {
            (port == null ? (Port)IOPorts.Stdout.Get() : port).Flush();
        }

        // 6.14 System interface

        [SchemeBridge]
        public static string[] Features() {
            return Interpreter.Features.ToArray();
        }

    }

}
