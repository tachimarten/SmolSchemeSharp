// SmolScheme/SchemeObject.cs

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Runtime.Serialization;
using System.Text;
using System.Threading;

namespace SmolScheme {
    public abstract class SchemeObject : SExpr {
        [SchemeBridge("Eqv?")]
        public abstract bool Eqv(SchemeObject other);

        public readonly int mID;

        private static int NextID = 0;

        internal sealed override SchemeObject Quote() {
            return this;
        }

        public SchemeObject() : base(null) {
            mID = Interlocked.Increment(ref NextID) - 1;
        }

        public sealed override string ToString(ToStringOptions options) {
            if (!options.Labels.ContainsKey(this))
                return StructureToString(options);

            if (options.Visited.Contains(this))
                return "#" + options.Labels[this] + "#";

            ToStringOptions newOptions = options.DuplicateHavingVisited(this);
            return String.Format("#{0}={1}", options.Labels[this], StructureToString(newOptions));
        }

        public override sealed string ToString() {
            var cycles = new SchemeObjectPtrSet();
            FindCycles(new SchemeObjectPtrSet(), cycles);
            return ToString(new ToStringOptions(ToStringFlags.ShouldBeQuoted, cycles));
        }

        public abstract string StructureToString(ToStringOptions options);

        public virtual bool IsTruthy {
            get { return true; }
        }

        public bool IsFalsy {
            get { return !IsTruthy; }
        }

        /// <summary>
        /// Bridges from a .NET type to a Scheme type.
        /// </summary>
        public static SchemeObject Bridge(object obj) {
            if (obj == null)
                return SchemeNull.Null;

            // Handle basic types.
            Type objType = obj.GetType();
            switch (Type.GetTypeCode(objType)) {
                case TypeCode.Boolean: return SchemeBool.Create((bool)obj);
                case TypeCode.Byte: return SchemeNumber.Create((byte)obj);
                case TypeCode.Char: return new SchemeChar((char)obj);
                case TypeCode.Double: return SchemeNumber.Create((double)obj);
                case TypeCode.Int16: return SchemeNumber.Create((short)obj);
                case TypeCode.Int32: return SchemeNumber.Create((int)obj);
                case TypeCode.Int64: return SchemeNumber.Create((long)obj);
                case TypeCode.SByte: return SchemeNumber.Create((sbyte)obj);
                case TypeCode.Single: return SchemeNumber.Create((float)obj);
                case TypeCode.String: return new SchemeString((string)obj);
                case TypeCode.UInt16: return SchemeNumber.Create((ushort)obj);
                case TypeCode.UInt32: return SchemeNumber.Create((uint)obj);
                case TypeCode.UInt64: return SchemeNumber.Create((ulong)obj);
                case TypeCode.DateTime:
                case TypeCode.Decimal:
                case TypeCode.Object:
                    break;
                case TypeCode.DBNull:
                case TypeCode.Empty:
                    return SchemeNull.Null;
            }

            // Scheme objects are passed unmodified.
            if (obj is SchemeObject)
                return (SchemeObject)obj;

            // Complex numbers become inexact Scheme numbers.
            if (obj is Complex)
                return SchemeNumber.Create((Complex)obj);

            // Arrays become vectors, except for arrays of bytes, which become bytevectors.
            if (obj is Array) {
                if (objType.GetElementType() == typeof(byte))
                    return new Bytevector((byte[])obj);

                var array = (Array)obj;
                SchemeObject[] bridged = new SchemeObject[array.Length];
                for (int i = 0; i < array.Length; i++)
                    bridged[i] = SchemeObject.Bridge(array.GetValue(i));
                return new Vector(bridged);
            }

            // Dictionaries become association lists.
            if (obj is IDictionary) {
                IDictionary dictionary = (IDictionary)obj;
                SchemeObject[] bridged = new SchemeObject[dictionary.Count];
                int i = 0;
                foreach (DictionaryEntry entry in dictionary) {
                    SchemeObject bridgedKey = SchemeObject.Bridge(entry.Key);
                    SchemeObject bridgedValue = SchemeObject.Bridge(entry.Value);
                    bridged[i++] = new SchemeList(bridgedKey, bridgedValue);
                }
                return SchemeList.CreateList(bridged);
            }

            // Otherwise, wrap in a NativeObject wrapper.
            return new NativeObject(obj);
        }

        /// <summary>
        /// Attempts to bridge from a Scheme type to a CLR type, returning true on success or
        /// false on failure.
        /// </summary>
        public virtual bool TryBridgeTo(Type type, out object outResult) {
            bool ok;
            outResult = ((ok = type.IsAssignableFrom(this.GetType()))) ? this : null;
            return ok;
        }

        /// <summary>
        /// Attempts to bridge from a Scheme type to a CLR type, returning true on success or
        /// false on failure. Generic version.
        /// </summary>
        public bool TryBridgeTo<T>(out T outResult) {
            object result;
            if (!TryBridgeTo(typeof(T), out result)) {
                outResult = default(T);
                return false;
            }
            outResult = (T)result;
            return true;
        }

        /// <summary>
        /// Bridges from a Scheme type to a CLR type, throwing a SchemeException on failure.
        /// </summary>
        public object BridgeTo(Type type) {
            object result = null;
            if (!TryBridgeTo(type, out result)) {
                throw new SchemeException(String.Format(
                    "Scheme type {0} isn't convertible to the native type {1}", GetType(), type));
            }
            return result;
        }

        /// <summary>
        /// Bridges from a Scheme type to a CLR type, throwing a SchemeException on failure.
        /// Generic version.
        /// </summary>
        public T BridgeTo<T>() {
            return (T)BridgeTo(typeof(T));
        }

        internal virtual bool Equal(SchemeObject other, SchemeObjectPtrSet visited) {
            return Eqv(other);
        }

        [SchemeBridge("Equal?")]
        public bool Equal(SchemeObject other) {
            return Equal(other, new SchemeObjectPtrSet());
        }

        /// <summary>
        /// This is the same as `Eqv?`, which is allowed by R⁷RS § 6.1.
        /// </summary>
        [SchemeBridge("Eq?")]
        public bool Eq(SchemeObject other) {
            return Eqv(other);
        }

        /// <summary>
        /// Returns true if this is the first time we've seen this object or false if we've seen it
        /// before.
        /// </summary>
        protected bool MarkVisited(SchemeObjectPtrSet visited, SchemeObjectPtrSet cycles) {
            bool alreadyVisited = visited.Contains(this);
            if (!alreadyVisited)
                visited.Add(this);
            else
                cycles.Add(this);
            return !alreadyVisited;

        }

        internal virtual void FindCycles(SchemeObjectPtrSet visited, SchemeObjectPtrSet cycles) {
            MarkVisited(visited, cycles);
        }

        internal static string PrefixToString(string name, ToStringOptions options) {
            if (options.ContainsFlags(ToStringFlags.MJSExprForm))
                return String.Format("{0}[", name);
            return String.Format("({0}", Identifier.SwitchRepr(name), name);
        }

        internal static string SuffixToString(ToStringOptions options) {
            return options.ContainsFlags(ToStringFlags.MJSExprForm) ? "]" : ")";
        }
    }

    [SchemeBridge.TypeTest("Null?")]
    public sealed class SchemeNull : SchemeObject {
        private SchemeNull() : base() { }

        public static readonly SchemeNull Null = new SchemeNull();

        public override string StructureToString(ToStringOptions options) {
            return !options.ContainsFlags(ToStringFlags.MJSExprForm) ? "'()" : "'{}";
        }

        internal override SExpr Unquote() {
            return this;
        }

        public override bool Eqv(SchemeObject other) {
            return other is SchemeNull;
        }

        internal override SExpr ReplaceIdentifiers(Dictionary<string, SExpr> replacements) {
            return this;
        }

        internal override void RecursivelyReplaceDatumLabel(int label, SExpr replacement) { }

        public override bool TryBridgeTo(Type type, out object outResult) {
            if (type == typeof(void)) {
                outResult = null;
                return true;
            }

            if (type.IsValueType && Nullable.GetUnderlyingType(type) == null)
                return base.TryBridgeTo(type, out outResult);

            outResult = null;
            return true;
        }
    }

    internal class CaseLambdaClause {
        public readonly ProcArgs Args;
        public readonly SExpr Body;

        public CaseLambdaClause(ProcArgs args, SExpr body) {
            Args = args;
            Body = body;
        }

        public string ToString(ToStringOptions options) {
            ToStringOptions bodyOptions = options.DuplicateWithoutFlags(ToStringFlags.PartOfList);
            if (!options.ContainsFlags(ToStringFlags.MJSExprForm))
                return Args + " " + Body.ToString(bodyOptions);
            return Args + ", " + Body.ToString(bodyOptions);
        }
    }

    internal abstract class ProcArgs {
        public abstract string StructureToString(ToStringOptions options);
    }

    internal sealed class NormalProcArgs : ProcArgs {
        public readonly string[] Args;

        public NormalProcArgs(string[] args) {
            Args = args;
        }

        public override string StructureToString(ToStringOptions options) {
            if (!options.ContainsFlags(ToStringFlags.MJSExprForm))
                return "(" + String.Join(" ", Args) + ")";
            return "{" + String.Join(", ", Args) + "}";
        }
    }

    internal sealed class VariadicProcArgs : ProcArgs {
        public readonly string[] Args;
        public readonly string VariadicArgs;

        public VariadicProcArgs(string[] args, string variadicArgs) {
            Args = args;
            VariadicArgs = variadicArgs;
        }

        public override string StructureToString(ToStringOptions options) {
            if (Args.Length == 0)
                return VariadicArgs;
            if (!options.ContainsFlags(ToStringFlags.MJSExprForm))
                return String.Format("({0} . {1})", String.Join(" ", Args), VariadicArgs);
            return String.Format("{{{0}, ...{1}}}", String.Join(", ", Args), VariadicArgs);
        }
    }

    public sealed class Record : SchemeObject {
        public readonly RecordType Type;
        public readonly SchemeObject[] Fields;

        public Record(RecordType type, SchemeObject[] fields) {
            Type = type;
            Fields = fields;
        }

        public override string StructureToString(ToStringOptions options) {
            ToStringOptions fieldOptions = options.DuplicateWithoutFlags(ToStringFlags.PartOfList);

            StringBuilder builder = new StringBuilder();
            builder.Append(Type.ConstructorName);
            builder.Append(options.ContainsFlags(ToStringFlags.MJSExprForm) ? '[' : '(');

            for (int i = 0; i < Fields.Length; i++) {
                if (i > 0)
                    builder.Append(options.ContainsFlags(ToStringFlags.MJSExprForm) ? ", " : " ");
                builder.AppendFormat(
                    "{0}: {1}", Type.FieldIndexToName[i], Fields[i].ToString(fieldOptions));
            }

            builder.Append(options.ContainsFlags(ToStringFlags.MJSExprForm) ? ']' : ')');
            return "" + builder;
        }

        public override bool Eqv(SchemeObject other) {
            return this == other;
        }

        internal override void FindCycles(SchemeObjectPtrSet visited, SchemeObjectPtrSet cycles) {
            if (MarkVisited(visited, cycles)) {
                foreach (SchemeObject field in Fields)
                    field.FindCycles(visited, cycles);
            }
        }
    }

    [SchemeBridge.TypeTest("ErrorObject?")]
    public class Error : SchemeObject {
        [SchemeBridge.Accessor("ErrorObjectMessage")]
        public readonly string Message;
        public readonly SchemeObject[] Irritants;
        public List<SourceLocation> SchemeStackTrace { get; private set; }

        public Error(string message, SchemeObject[] irritants, SourceLocation loc = null) :
                base() {
            Message = message;
            Irritants = irritants == null ? new SchemeObject[] { } : irritants;

            SchemeStackTrace = new List<SourceLocation>();
            if (loc != null)
                SchemeStackTrace.Add(loc);
        }

        public Error(string message) : this(message, new SchemeObject[] { }, null) { }

        public override string StructureToString(ToStringOptions options) {
            StringBuilder builder = new StringBuilder();
            builder.Append(Message);

            if (SchemeStackTrace.Count > 0) {
                builder.AppendFormat("\nScheme stack trace:");
                foreach (SourceLocation loc in SchemeStackTrace)
                    builder.AppendFormat("\n  at {0}", loc);
            }

            return "" + builder;
        }

        public override bool Eqv(SchemeObject other) {
            return this == other;
        }

        public void AddSchemeFrame(SourceLocation loc) {
            SchemeStackTrace.Add(loc);
        }

        [SchemeBridge("Error")]
        public static void CreateAndThrow(string message, params SchemeObject[] irritants) {
            throw new SchemeException(new Error(message, irritants));
        }

        [SchemeBridge("ErrorObjectIrritants")]
        public SchemeObject IrritantsToList() {
            return SchemeList.CreateList(Irritants);
        }

        internal override void FindCycles(SchemeObjectPtrSet visited, SchemeObjectPtrSet cycles) {
            if (MarkVisited(visited, cycles)) {
                foreach (SchemeObject irritant in Irritants)
                    irritant.FindCycles(visited, cycles);
            }
        }
    }

    [SchemeBridge.TypeTest]
    public class ReadError : Error {
        public ReadError(string message) : base(message) { }
    }

    [SchemeBridge.TypeTest]
    public class FileError : Error {
        public FileError(string message) : base(message) { }
    }

    [SchemeBridge.TypeTest]
    public abstract class Port : SchemeObject, IDisposable {
        public abstract bool IsInput { get; }
        public abstract bool IsOutput { get; }

        public abstract bool IsInputOpen {
            [SchemeBridge("InputPortOpen?")]
            get;
        }

        public abstract bool IsOutputOpen {
            [SchemeBridge("OutputPortOpen?")]
            get;
        }

        public Port() : base() { }

        public override bool Eqv(SchemeObject other) {
            return this == other;
        }

        public abstract void Close();

        [SchemeBridge("ClosePort")]
        public virtual void Dispose() { }

        [SchemeBridge]
        public void CloseInputPort() {
            Dispose();
        }

        [SchemeBridge]
        public void CloseOutputPort() {
            Dispose();
        }

        protected string ToStringPrefix {
            get {
                string description = "";
                if (IsInput)
                    description += "Input";
                if (IsOutput)
                    description += "Output";
                return description;
            }
        }

        public abstract void Flush();
    }

    [SchemeBridge.TypeTest]
    public class BinaryPort : Port {
        // Null when closed.
        private PeekableStream mStream;

        public BinaryPort(Stream stream) : base() {
            mStream = new PeekableStream(stream);
        }

        public override bool IsInput {
            get { return mStream.CanRead; }
        }

        public override bool IsOutput {
            get { return mStream.CanWrite; }
        }

        public override bool IsInputOpen {
            get { return mStream != null; }
        }

        public override bool IsOutputOpen {
            get { return mStream != null; }
        }

        public override string StructureToString(ToStringOptions options) {
            return ToStringPrefix + "BinaryPort";
        }

        public override void Close() {
            mStream.Close();
            mStream = null;
        }

        public override void Dispose() {
            base.Dispose();
            mStream.Dispose();
        }

        public override void Flush() {
            mStream.Flush();
        }

        public int Read(byte[] buffer, int offset, int count) {
            return mStream.Read(buffer, offset, count);
        }

        public int ReadAll(byte[] buffer, int offset = 0, int count = -1) {
            if (count < 0)
                count = buffer.Length;

            int totalRead = 0;
            int nRead;
            while ((nRead = mStream.Read(buffer, totalRead + offset, count - totalRead)) != 0)
                totalRead += nRead;
            return totalRead;
        }

        public int ReadByte() {
            return mStream.ReadByte();
        }

        public int Peek() {
            return mStream.Peek();
        }

        public void Write(byte[] buffer, int offset, int count) {
            mStream.Write(buffer, offset, count);
        }

        public void WriteByte(byte value) {
            mStream.WriteByte(value);
        }
    }

    public class BytevectorPort : BinaryPort {
        private MemoryStream mMemoryStream;

        public BytevectorPort(MemoryStream stream) : base(stream) {
            mMemoryStream = stream;
        }

        [SchemeBridge("OpenInputBytevector")]
        public BytevectorPort(Bytevector bytevector) :
                this(new MemoryStream(bytevector.Elements)) { }

        [SchemeBridge("OpenOutputBytevector")]
        public BytevectorPort() : this(new MemoryStream()) { }

        public byte[] Buffer {
            get { return mMemoryStream.GetBuffer(); }
        }

        public Bytevector OutputBytevector {
            [SchemeBridge("GetOutputBytevector")]
            get { return new Bytevector(Buffer); }
        }
    }

    [SchemeBridge.TypeTest]
    public class TextualPort : Port {
        // TODO: Buffer?

        // Null when closed.
        public TextReader Reader;
        // Null when closed.
        public TextWriter Writer;

        private bool EverHadReader;
        private bool EverHadWriter;

        public TextualPort(TextReader reader, TextWriter writer) : base() {
            Reader = reader;
            Writer = writer;
            EverHadReader = Reader != null;
            EverHadWriter = Writer != null;
        }

        public override bool IsInput {
            get { return EverHadReader; }
        }

        public override bool IsOutput {
            get { return EverHadWriter; }
        }

        public override bool IsInputOpen {
            get { return Reader != null; }
        }

        public override bool IsOutputOpen {
            get { return Writer != null; }
        }

        public override string StructureToString(ToStringOptions options) {
            return ToStringPrefix + "TextualPort";
        }

        public override void Close() {
            if (Reader != null) {
                Reader.Close();
                Reader = null;
            }
            if (Writer != null) {
                Writer.Close();
                Writer = null;
            }
        }

        public override void Dispose() {
            base.Dispose();
            if (Reader != null)
                Reader.Dispose();
            if (Writer != null)
                Writer.Dispose();
        }

        public override void Flush() {
            if (Writer != null)
                Writer.Flush();
        }

        public int Peek() {
            return Reader.Peek();
        }

        public int Read() {
            return Reader.Read();
        }

        public int ReadBlock(char[] buffer, int index, int count) {
            return Reader.ReadBlock(buffer, index, count);
        }

        public string ReadLine() {
            return Reader.ReadLine();
        }

        public void Write(char value) {
            Writer.Write(value);
        }

        public void Write(string value) {
            Writer.Write(value);
        }

        public void WriteLine() {
            Writer.WriteLine();
        }

        [SchemeBridge("CharReady?")]
        public static bool CharReady(TextualPort port = null) {
            if (port == null)
                port = (TextualPort)IOPorts.Stdin.Get();
            if (port.Reader == Console.In && !Console.IsInputRedirected)
                return Console.KeyAvailable;
            return port.Peek() >= 0;
        }
    }

    public class StreamTextualPort : TextualPort {
        public readonly Stream Stream;

        public StreamTextualPort(Stream stream) : base(
            stream.CanRead ? new StreamReader(stream) : null,
            stream.CanWrite ? new StreamWriter(stream) : null
        ) {
            Stream = stream;
        }

        public override void Dispose() {
            base.Dispose();
            Stream.Dispose();
        }
    }

    public class StringInputTextualPort : TextualPort {
        [SchemeBridge("OpenInputString")]
        public StringInputTextualPort(string str) : base(new StringReader(str), null) { }
    }

    public class StringOutputTextualPort : TextualPort {
        private StringWriter mStringWriter;

        [SchemeBridge("OpenOutputString")]
        private StringOutputTextualPort(StringWriter stringWriter) :
                base(null, stringWriter) {
            mStringWriter = stringWriter;
        }

        public StringOutputTextualPort() : this(new StringWriter()) { }

        public string String {
            [SchemeBridge("GetOutputString")]
            get { return "" + mStringWriter; }
        }
    }

    [SchemeBridge.TypeTest]
    public sealed class EofObject : SchemeObject {
        private EofObject() : base() { }

        public static readonly EofObject Eof = new EofObject();

        public override string StructureToString(ToStringOptions options) {
            if (options.ContainsFlags(ToStringFlags.MJSExprForm))
                return "EofObject[]";
            return "(eof-object)";
        }

        public override bool Eqv(SchemeObject other) {
            return other is EofObject;
        }

        internal override SExpr ReplaceIdentifiers(Dictionary<string, SExpr> replacements) {
            return this;
        }

        internal override void RecursivelyReplaceDatumLabel(int label, SExpr replacement) { }
    }

    // 4.2.5 Delayed evaluation

    public sealed class Promise : SchemeObject {
        public PromisePayload Payload;

        public Promise(PromisePayload payload) {
            Payload = payload;
        }

        public override bool Eqv(SchemeObject other) {
            return this == other;
        }

        public override string StructureToString(ToStringOptions options) {
            ToStringOptions payloadOptions =
                options.DuplicateWithoutFlags(ToStringFlags.PartOfList);

            return String.Join("", new string[] {
                PrefixToString("Promise", options),
                Payload.ToString(payloadOptions),
                SuffixToString(options),
            });
        }

        public abstract class PromisePayload {
            public abstract string ToString(ToStringOptions options);
            public virtual void FindCycles(
                SchemeObjectPtrSet visited, SchemeObjectPtrSet cycles) { }
        }

        public class UnresolvedPayload : PromisePayload {
            public readonly SExpr Expr;
            public readonly Env Env;

            public UnresolvedPayload(SExpr expr, Env env) {
                Expr = expr;
                Env = env;
            }

            public override string ToString(ToStringOptions options) {
                ToStringOptions payloadOptions =
                    options.DuplicateWithoutFlags(ToStringFlags.PartOfList);
                return Expr.ToString(payloadOptions);
            }
        }

        public class ResolvedPayload : PromisePayload {
            public readonly SchemeObject Result;

            public ResolvedPayload(SchemeObject result) {
                Result = result;
            }

            public override string ToString(ToStringOptions options) {
                ToStringOptions payloadOptions =
                    options.DuplicateWithoutFlags(ToStringFlags.PartOfList);
                return Result.ToString(options);
            }

            public virtual void FindCycles(SchemeObjectPtrSet visited, SchemeObjectPtrSet cycles) {
                Result.FindCycles(visited, cycles);
            }
        }

        public class DelayForcePayload : PromisePayload {
            public readonly SExpr Expr;
            public readonly Env Env;

            public DelayForcePayload(SExpr expr, Env env) {
                Expr = expr;
                Env = env;
            }

            public override string ToString(ToStringOptions options) {
                ToStringOptions payloadOptions =
                    options.DuplicateWithoutFlags(ToStringFlags.PartOfList);

                return String.Join("", new string[] {
                    SchemeObject.PrefixToString("DelayForce", options),
                    Expr.ToString(payloadOptions),
                    SchemeObject.SuffixToString(options),
                });
            }
        }
    }

    // 6.12 Environments and evaluation

    public class EnvObject : SchemeObject {
        public Env Env;

        public EnvObject(Env env) {
            Env = env;
        }

        public override string StructureToString(ToStringOptions options) {
            return String.Join("", new string[3] {
                PrefixToString("Environment", options),
                Env.ToString(options),
                SuffixToString(options),
            });
        }

        public override bool Eqv(SchemeObject other) {
            return this == other;
        }
    }

    /// <summary>
    /// Wraps a CLR type so that Scheme can interact with it.
    /// </summary>
    public class NativeObject : SchemeObject {
        public readonly object Value;

        public NativeObject(object value) : base() {
            Value = value;
        }

        public T Get<T>() {
            if (Value == null)
                return default(T);
            if (!(Value is T)) {
                throw new SchemeException("Foreign value of type " + typeof(T) +
                    " requested but value is of type " + Value.GetType());
            }
            return (T)Value;
        }

        public override string StructureToString(ToStringOptions options) {
            return "" + Value;
        }

        public override bool Eqv(SchemeObject other) {
            return this == other;
        }

        public override bool TryBridgeTo(Type type, out object outResult) {
            if (type.IsAssignableFrom(Value.GetType())) {
                outResult = Value;
                return true;
            }
            return base.TryBridgeTo(type, out outResult);
        }
    }

    // Bridging utilities

    public abstract class SchemeBridgeAttribute : Attribute {
        public string Name;

        public SchemeBridgeAttribute(string name) {
            Name = name;
        }
    }

    public class SchemeBridge : SchemeBridgeAttribute {
        public SchemeBridge(string name = null) : base(name) { }

        public class TypeTest : SchemeBridgeAttribute {
            public TypeTest(string name = null) : base(name) { }
        }

        public class Accessor : SchemeBridgeAttribute {
            public Accessor(string name = null) : base(name) { }
        }

        public class Mutator : SchemeBridgeAttribute {
            public Mutator(string name = null) : base(name) { }
        }
    }

    // Other utilities

    public class SchemeObjectPtrSet : IEnumerable<SchemeObject> {
        private Dictionary<int, List<SchemeObject>> mSet;

        public SchemeObjectPtrSet() {
            mSet = new Dictionary<int, List<SchemeObject>>();
        }

        public bool Add(SchemeObject datum) {
            if (!mSet.ContainsKey(datum.mID))
                mSet.Add(datum.mID, new List<SchemeObject>());

            if (mSet[datum.mID].Contains(datum))
                return false;
            mSet[datum.mID].Add(datum);
            return true;
        }

        public bool Contains(SchemeObject datum) {
            return mSet.ContainsKey(datum.mID) && mSet[datum.mID].Contains(datum);
        }

        public SchemeObjectPtrSet Clone() {
            SchemeObjectPtrSet clone = new SchemeObjectPtrSet();
            foreach (KeyValuePair<int, List<SchemeObject>> pair in mSet) {
                foreach (SchemeObject obj in pair.Value)
                    clone.Add(obj);
            }
            return clone;
        }

        IEnumerator IEnumerable.GetEnumerator() {
            return ((IEnumerable<SchemeObject>)this).GetEnumerator();
        }

        IEnumerator<SchemeObject> IEnumerable<SchemeObject>.GetEnumerator() {
            return mSet.SelectMany(pair => pair.Value).GetEnumerator();
        }

        public override string ToString() {
            return "(" + String.Join(", ", this.Select(x => "" + x.mID)) + ")";
        }
    }

    public class SchemeObjectPtrMap<T> {
        private Dictionary<int, List<KeyValuePair<SchemeObject, T>>> mMap;

        public SchemeObjectPtrMap() {
            mMap = new Dictionary<int, List<KeyValuePair<SchemeObject, T>>>();
        }

        public bool Add(SchemeObject datum, T value) {
            if (!mMap.ContainsKey(datum.mID))
                mMap.Add(datum.mID, new List<KeyValuePair<SchemeObject, T>>());

            if (mMap[datum.mID].Any(pair => pair.Key == datum))
                return false;
            mMap[datum.mID].Add(new KeyValuePair<SchemeObject, T>(datum, value));
            return true;
        }

        public bool ContainsKey(SchemeObject datum) {
            if (!mMap.ContainsKey(datum.mID))
                return false;

            foreach (KeyValuePair<SchemeObject, T> pair in mMap[datum.mID]) {
                if (pair.Key == datum)
                    return true;
            }
            return false;
        }

        public T this[SchemeObject datum] {
            get {
                if (!mMap.ContainsKey(datum.mID))
                    return default(T);

                foreach (KeyValuePair<SchemeObject, T> pair in mMap[datum.mID]) {
                    if (pair.Key == datum)
                        return pair.Value;
                }
                return default(T);
            }
        }
    }
}
