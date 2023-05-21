// SmolScheme/SExpr.cs

using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;
using System.Text;

namespace SmolScheme {
    // S-expressions.
    public abstract class SExpr {
        internal SourceLocation Loc;

        internal abstract SchemeObject Quote();

        internal virtual SExpr Unquote() {
            throw new SchemeException("Can't eval an object of type " + GetType());
        }

        public abstract string ToString(ToStringOptions options);

        internal virtual SExpr ReplaceIdentifiers(Dictionary<string, SExpr> replacements) {
            throw new SchemeException(
                "Can't replace identifiers in an object of type " + GetType());
        }

        internal virtual void RecursivelyReplaceDatumLabel(int label, SExpr replacement) {
            throw new SchemeException(
                "Can't replace datum labels in an object of type " + GetType());
        }

        protected static void ReplaceDatumLabel(ref SExpr refExpr, int label, SExpr replacement) {
            if (refExpr is LabelRefDatum && ((LabelRefDatum)refExpr).Label == label)
                refExpr = replacement;
        }

        protected SExpr(SourceLocation loc = null) {
            Loc = loc;
        }

        public override string ToString() {
            return ToString(new ToStringOptions((ToStringFlags)0));
        }

        public int RequireExactInt() {
            return Require<SchemeNumber>().BridgeTo<int>();
        }

        public byte RequireExactByte() {
            return Require<SchemeNumber>().BridgeTo<byte>();
        }

        public string RequireIdentifier() {
            if (!(this is Identifier))
                throw new SchemeException("Expected symbol expression but found " + GetType());
            return ((Identifier)this).Name;
        }

        public IEnumerable<SExpr> RequireListExpr(int minCount = 0, int maxCount = -1) {
            if (this is SchemeNull)
                return new SchemeList[] { };
            if (!(this is ListExpr))
                throw new SchemeException("Expected list but found " + GetType());

            var listExpr = (ListExpr)this;
            if (!SExprUtils.CheckArity(listExpr.Length, minCount, maxCount)) {
                SExprUtils.ReportBoundsError(
                    "Expected list of length ",
                    listExpr.Length,
                    minCount,
                    maxCount,
                    "element");
            }
            return listExpr;
        }

        public T Require<T>() where T : SExpr {
            if (this is T)
                return (T)this;
            throw new SchemeException("Expected " + typeof(T) + " but found " + GetType());
        }

        public Record RequireRecord(RecordType recordType) {
            Record Record = Require<Record>();
            if (Record.Type != recordType) {
                throw new SchemeException("Expected record of type " + recordType.Name +
                    " but found record of type " + Record.Type);
            }
            return Record;
        }

        public string RequireString() {
            if (this is SchemeString)
                return ((SchemeString)this).Value;
            throw new SchemeException("Expected string but found " + GetType());
        }
    }

    /// <summary>
    /// An improper list.
    ///
    /// NB: Enumerating this does not enumerate the tail too.
    /// </summary>
    public class SplatExpr : SExpr, IEnumerable<SExpr> {
        public SExpr Head;
        public SExpr[] Body;
        public SExpr Tail;

        public SplatExpr(
                SExpr head, SExpr[] body, SExpr tail, SourceLocation loc = null) : base(loc) {
            Head = head;
            Body = body;
            Tail = tail;
        }

        public SplatExpr(SExpr head, SExpr tail, SourceLocation loc = null) : base(loc) {
            Head = head;
            Body = new SExpr[] { };
            Tail = tail;
        }

        internal override SchemeObject Quote() {
            SchemeObject tail = Tail.Quote();
            for (int i = Body.Length - 1; i >= 0; i--)
                tail = new SchemeList(Body[i].Quote(), tail);
            return new SchemeList(Head.Quote(), tail);
        }

        internal override SExpr Unquote() {
            return new SplatExpr(
                Head.Unquote(), Body.Select(expr => expr.Unquote()).ToArray(), Tail, Loc);
        }

        public override string ToString(ToStringOptions options) {
            if (options.ShouldBeQuotedButIsnt) {
                return "'" + ToString(options.DuplicateWithFlags(
                    ToStringFlags.ShouldBeQuoted | ToStringFlags.AlreadyQuoted));
            }

            return Head.ToString(options) +
                "[" +
                String.Join(", ", Body.Select(expr => expr.ToString(options))) +
                "..." +
                Tail.ToString(options) +
                "]";
        }

        internal override SExpr ReplaceIdentifiers(Dictionary<string, SExpr> replacements) {
            return new SplatExpr(
                Head.ReplaceIdentifiers(replacements),
                Body.Select(expr => expr.ReplaceIdentifiers(replacements)).ToArray(),
                Tail.ReplaceIdentifiers(replacements));
        }

        internal override void RecursivelyReplaceDatumLabel(int label, SExpr replacement) {
            ReplaceDatumLabel(ref Head, label, replacement);
            Head.RecursivelyReplaceDatumLabel(label, replacement);

            for (int i = 0; i < Body.Length; i++) {
                ReplaceDatumLabel(ref Body[i], label, replacement);
                Body[i].RecursivelyReplaceDatumLabel(label, replacement);
            }

            ReplaceDatumLabel(ref Tail, label, replacement);
            Tail.RecursivelyReplaceDatumLabel(label, replacement);
        }

        IEnumerator IEnumerable.GetEnumerator() {
            return ((IEnumerable<SExpr>)this).GetEnumerator();
        }

        IEnumerator<SExpr> IEnumerable<SExpr>.GetEnumerator() {
            return (new SExpr[] { Head }.Concat(Body)).GetEnumerator();
        }
    }

    public class ListExpr : SExpr, IEnumerable<SExpr> {
        public SExpr Head;
        public readonly SExpr[] Tail;

        public int Length {
            get { return Tail.Length + 1; }
        }

        public ListExpr(SExpr head, SExpr[] tail, SourceLocation loc = null) : base(loc) {
            Head = head;
            Tail = tail;
        }

        public ListExpr(string headSymbol, SExpr[] tail, SourceLocation loc = null) : base(loc) {
            Head = new Identifier(headSymbol, IdentifierRepr.MJSRepr, loc);
            Tail = tail;
        }

        public ListExpr(SExpr head, IEnumerable<SExpr> tail, SourceLocation loc = null) :
                base(loc) {
            Head = head;
            Tail = tail.ToArray();
        }

        public ListExpr(string headSymbol, IEnumerable<SExpr> tail, SourceLocation loc = null) :
                base(loc) {
            Head = new Identifier(headSymbol, IdentifierRepr.MJSRepr, loc);
            Tail = tail.ToArray();
        }

        public ListExpr(IEnumerable<SExpr> elements, SourceLocation loc = null) : base(loc) {
            Head = elements.First();
            Tail = elements.Skip(1).ToArray();
        }

        public ListExpr(SExpr head, params SExpr[] tail) : base(null) {
            Head = head;
            Tail = tail;
        }

        public ListExpr(string headSymbol, params SExpr[] tail) : base(null) {
            Head = new Identifier(headSymbol, IdentifierRepr.MJSRepr);
            Tail = tail;
        }

        internal SExpr GetTailExpr() {
            if (Tail.Length == 0)
                return SchemeNull.Null;
            return new ListExpr(Tail, Loc);
        }

        internal override SchemeObject Quote() {
            return new SchemeList(Head.Quote(), Tail.Select(elem => elem.Quote()));
        }

        internal override SExpr Unquote() {
            return new ListExpr(Head.Unquote(), Tail.Select(expr => expr.Unquote()), Loc);
        }

        public override string ToString(ToStringOptions options) {
            if (options.ShouldBeQuotedButIsnt) {
                return "'" + ToString(options.DuplicateWithFlags(
                    ToStringFlags.ShouldBeQuoted | ToStringFlags.AlreadyQuoted));
            }

            return Head.ToString(options) +
                "[" +
                String.Join(", ", Tail.Select(value => value.ToString(options))) +
                "]";
        }

        IEnumerator IEnumerable.GetEnumerator() {
            return ((IEnumerable<SExpr>)this).GetEnumerator();
        }

        IEnumerator<SExpr> IEnumerable<SExpr>.GetEnumerator() {
            return (new SExpr[] { Head }.Concat(Tail)).GetEnumerator();
        }

        public SExpr this[int i] {
            get {
                return i == 0 ? Head : Tail[i - 1];
            }
        }

        internal override SExpr ReplaceIdentifiers(Dictionary<string, SExpr> replacements) {
            return new ListExpr(
                Head.ReplaceIdentifiers(replacements),
                Tail.Select(expr => expr.ReplaceIdentifiers(replacements)).ToArray(),
                Loc);
        }

        internal override void RecursivelyReplaceDatumLabel(int label, SExpr replacement) {
            ReplaceDatumLabel(ref Head, label, replacement);
            Head.RecursivelyReplaceDatumLabel(label, replacement);

            for (int i = 0; i < Tail.Length; i++) {
                ReplaceDatumLabel(ref Tail[i], label, replacement);
                Tail[i].RecursivelyReplaceDatumLabel(label, replacement);
            }
        }
    }

    /// <summary>
    /// This represents `()`. Evaluating this is always an error; it exists solely to be quoted.
    /// </summary>
    public class NullExpr : SExpr {
        public NullExpr(SourceLocation loc = null) : base(loc) {}

        internal override SchemeObject Quote() {
            return SchemeNull.Null;
        }

        public override string ToString(ToStringOptions options) {
            if (options.ContainsFlags(ToStringFlags.MJSExprForm))
                return "{}";
            return "()";
        }
    }

    /// <summary>
    /// Both vector and bytevector expressions.
    /// </summary>
    public abstract class BaseVectorExpr : SExpr, IEnumerable<SExpr> {
        public readonly SExpr[] Elements;

        public BaseVectorExpr(SExpr[] elements, SourceLocation loc = null) : base(loc) {
            Elements = elements;
        }

        public int Length {
            get { return Elements.Length; }
        }

        protected string ElementsToString(ToStringOptions options) {
            if (!options.ContainsFlags(ToStringFlags.MJSExprForm))
                return String.Join(" ", Elements.Select(elem => elem.ToString(options)).ToArray());
            return String.Join(", ", Elements.Select(elem => elem.ToString(options)).ToArray());
        }

        protected SExpr[] ReplaceIdentifiersInElements(Dictionary<string, SExpr> replacements) {
            return Elements.Select(element => element.ReplaceIdentifiers(replacements)).ToArray();
        }

        internal sealed override void RecursivelyReplaceDatumLabel(int label, SExpr replacement) {
            for (int i = 0; i < Length; i++) {
                ReplaceDatumLabel(ref Elements[i], label, replacement);
                Elements[i].RecursivelyReplaceDatumLabel(label, replacement);
            }
        }

        protected SchemeObject[] QuoteElements() {
            return Elements.Select(element => element.Quote()).ToArray();
        }

        protected SExpr[] UnquoteElements() {
            return Elements.Select(element => element.Unquote()).ToArray();
        }

        IEnumerator IEnumerable.GetEnumerator() {
            return ((IEnumerable<SExpr>)this).GetEnumerator();
        }

        IEnumerator<SExpr> IEnumerable<SExpr>.GetEnumerator() {
            return ((IEnumerable<SExpr>)Elements).GetEnumerator();
        }
    }

    public class VectorExpr : BaseVectorExpr {
        public VectorExpr(SExpr[] elements, SourceLocation loc = null) : base(elements, loc) { }

        public override string ToString(ToStringOptions options) {
            if (!options.ContainsFlags(ToStringFlags.MJSExprForm))
                return "#(" + ElementsToString(options) + ")";
            return "[" + ElementsToString(options) + "]";
        }

        internal override SExpr ReplaceIdentifiers(Dictionary<string, SExpr> replacements) {
            return new VectorExpr(ReplaceIdentifiersInElements(replacements));
        }

        internal override SchemeObject Quote() {
            return new Vector(QuoteElements());
        }

        internal override SExpr Unquote() {
            return new VectorExpr(UnquoteElements());
        }
    }

    public class BytevectorExpr : BaseVectorExpr {
        public BytevectorExpr(SExpr[] elements, SourceLocation loc = null) :
            base(elements, loc) { }

        public override string ToString(ToStringOptions options) {
            if (!options.ContainsFlags(ToStringFlags.MJSExprForm))
                return "#(" + ElementsToString(options) + ")";
            return "[" + ElementsToString(options) + "]";
        }

        internal override SExpr ReplaceIdentifiers(Dictionary<string, SExpr> replacements) {
            return new BytevectorExpr(ReplaceIdentifiersInElements(replacements));
        }

        internal override SchemeObject Quote() {
            return new Bytevector(QuoteElements().Select(
                elem => elem.RequireExactByte()).ToArray());
        }

        internal override SExpr Unquote() {
            return new BytevectorExpr(UnquoteElements());
        }
    }

    internal class Identifier : SExpr {
        public readonly string Name;

        public Identifier(string name, IdentifierRepr repr, SourceLocation loc = null) :
                base(loc) {
            Name = ToMJSForm(name, repr);
        }

        internal static string ToMJSForm(string name, IdentifierRepr repr) {
            switch (repr) {
                case IdentifierRepr.MJSRepr:
                    return name;
                case IdentifierRepr.SRepr:
                    return SwitchRepr(name);
                case IdentifierRepr.SReprFoldCase:
                    return SwitchRepr(name).ToLower();
            }
            throw new Exception("Unknown identifier repr");
        }

        internal override SchemeObject Quote() {
            return new Symbol(Name, IdentifierRepr.MJSRepr);
        }

        internal override SExpr Unquote() {
            return this;
        }

        public override string ToString(ToStringOptions options) {
            if (options.ShouldBeQuotedButIsnt) {
                return "'" + ToString(options.DuplicateWithFlags(
                    ToStringFlags.ShouldBeQuoted | ToStringFlags.AlreadyQuoted));
            }

            return IdentifierToString(Name, options);
        }

        internal static string IdentifierToString(string name, ToStringOptions options) {
            // S-expression form.
            if (!options.ContainsFlags(ToStringFlags.MJSExprForm)) {
                name = SwitchRepr(name);
                if (name.All(ch => SLexer.IsNonDigitIdentifierChar(ch) || Char.IsDigit(ch)) &&
                        !Char.IsDigit(name[0])) {
                    return name;
                }
                return SchemeString.Escape(name, '|');
            }

            // MJS-expression form.
            if (MJSLexer.IsFirstIdentifierChar(name[0]) &&
                    name.All(ch => MJSLexer.IsIdentifierChar(ch))) {
                return name;
            }
            return SchemeString.Escape(name, '`');

        }

        public string SRepr {
            get { return SwitchRepr(Name); }
        }

        private static char SwitchCase(char ch) {
            return Char.IsUpper(ch) ? Char.ToLower(ch) : Char.ToUpper(ch);
        }

        /// <summary>
        /// Switches the representation of the string from S-representation to MJS-representation
        /// or vice versa. (The operation is an involution: i.e. it's its own inverse.)
        /// </summary>
        public static string SwitchRepr(string inReprStr) {
            if (inReprStr.Length == 0)
                return inReprStr;

            char[] inRepr = inReprStr.ToCharArray();
            StringBuilder outRepr = new StringBuilder();
            outRepr.Append(SwapCase(inRepr[0]));

            for (int i = 1; i < inRepr.Length; i++) {
                char ch = inRepr[i];
                if (Char.IsUpper(ch)) {
                    outRepr.Append('-');
                    outRepr.Append(Char.ToLower(ch));
                } else if (ch == '-' && i + 1 < inRepr.Length && Char.IsLower(inRepr[i + 1])) {
                    outRepr.Append(Char.ToUpper(inRepr[i + 1]));
                    i++;
                } else if ((ch == '>' || ch == '/') && i + 1 < inRepr.Length) {
                    outRepr.Append(ch);
                    outRepr.Append(SwapCase(inRepr[i + 1]));
                    i++;
                } else {
                    outRepr.Append(ch);
                }
            }

            return "" + outRepr;
        }

        private static char SwapCase(char c) {
            return Char.IsLower(c) ? Char.ToUpper(c) : Char.ToLower(c);
        }

        internal override SExpr ReplaceIdentifiers(Dictionary<string, SExpr> replacements) {
            // TODO: Hygiene!
            if (replacements.ContainsKey(Name))
                return replacements[Name];
            return this;
        }

        internal override void RecursivelyReplaceDatumLabel(int label, SExpr replacement) { }
    }

    internal enum IdentifierRepr {
        MJSRepr = 0,
        SRepr = 1,
        SReprFoldCase = 2,
    }

    // Macros

    public abstract class Macro : Syntax {
        public abstract SExpr Call(SExpr[] args);

        public sealed override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                bool allowDefine) {
            return new Continuation(Call(listExpr.Tail), env, dynEnv, allowDefine);
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

    // Source locations

    public class SourceLocation {
        public readonly string Path;
        public readonly int LineNumber;

        public SourceLocation(string path, int lineNumber) {
            Path = path;
            LineNumber = lineNumber;
        }

        public override string ToString() {
            return Path == null ? "" + LineNumber : Path + ":" + LineNumber;
        }
    }

    // Record types

    public class RecordType {
        public readonly string Name;
        public readonly string ConstructorName;
        public readonly string[] FieldIndexToName;
        public readonly Dictionary<string, int> FieldNameToIndex;

        public RecordType(string name, string constructorName, string[] fieldNames) {
            Name = name;
            ConstructorName = constructorName;
            FieldIndexToName = fieldNames;

            FieldNameToIndex = new Dictionary<string, int>();
            for (int i = 0; i < fieldNames.Length; i++)
                FieldNameToIndex.Add(fieldNames[i], i);
        }

        public int FieldCount {
            get {
                return FieldIndexToName.Length;
            }
        }
    }

    // Utilities

    internal static class SExprUtils {
        internal static string Pluralize(string word, int count) {
            return "" + count + " " + word + (count == 1 ? "" : "s");
        }

        internal static bool CheckArity(int suppliedCount, int minCount, int maxCount) {
            bool hasMin = minCount > 0, hasMax = maxCount >= 0;
            return (!hasMin || suppliedCount >= minCount) &&
                (!hasMax || suppliedCount <= maxCount);
        }

        internal static void ReportBoundsError(
                string errorPrefix,
                int suppliedCount,
                int minCount,
                int maxCount,
                string elementName,
                SourceLocation loc = null) {
            bool hasMin = minCount > 0, hasMax = maxCount >= 0;

            string error = errorPrefix;
            if (hasMin && hasMax) {
                if (minCount == maxCount) {
                    error += "exactly " + Pluralize(elementName, minCount);
                } else {
                    error += "at least " + Pluralize(elementName, minCount) + " and at most " +
                        Pluralize(elementName, maxCount);
                }
            } else if (hasMin) {
                error += "at least " + Pluralize(elementName, minCount);
            } else if (hasMax) {
                error += "at most " + Pluralize(elementName, maxCount);
            } else {
                throw new Exception("Unreachable");
            }

            error += " but " + Pluralize(elementName, suppliedCount) +
                (suppliedCount == 1 ? " was" : " were") + " supplied";
            throw new SchemeException(error, loc);
        }

        internal static void ReportArityError(
                string kind,
                int suppliedArgCount,
                int minCount,
                int maxCount,
                SourceLocation loc = null) {
            ReportBoundsError(
                "This " + kind + " takes ",
                suppliedArgCount,
                minCount,
                maxCount,
                "argument");
        }
    }

    // Very loosely based on https://stackoverflow.com/a/7281113
    internal class PeekableStream : Stream {
        private readonly Stream mStream;
        private int mBuffer;

        const int EofObject = -1;
        const int EMPTY = -2;

        public PeekableStream(Stream stream) {
            mStream = stream;
            mBuffer = EMPTY;
        }

        protected override void Dispose(bool disposing) {
            if (disposing)
                mStream.Dispose();
            base.Dispose(disposing);
        }

        public int Peek() {
            if (mBuffer == EMPTY)
                mBuffer = mStream.ReadByte();
            return mBuffer;
        }

        public override long Position {
            get {
                return mBuffer >= 0 ? mStream.Position - 1 : mStream.Position;
            }
            set {
                mStream.Position = value;
                mBuffer = EMPTY;
            }
        }

        public override int Read(byte[] buffer, int offset, int count) {
            if (count == 0)
                return 0;

            if (mBuffer >= 0) {
                buffer[offset] = (byte)mBuffer;
                mBuffer = EMPTY;
                return mStream.Read(buffer, offset + 1, count - 1) + 1;
            }

            return mStream.Read(buffer, offset, count);
        }

        public override int ReadByte() {
            if (mBuffer == EMPTY)
                mBuffer = mStream.ReadByte();
            int value = mBuffer;
            if (value != EofObject)
                mBuffer = EMPTY;
            return value;
        }

        public override long Seek(long offset, SeekOrigin origin) {
            long result = mStream.Seek(offset, origin);
            mBuffer = EMPTY;
            return result;
        }

        public override void Write(byte[] buffer, int offset, int count) {
            mStream.Write(buffer, offset, count);
            mBuffer = EMPTY;
        }

        public override void WriteByte(byte value) {
            mStream.WriteByte(value);
            mBuffer = EMPTY;
        }

        public override bool CanRead { get { return mStream.CanRead; } }
        public override bool CanWrite { get { return mStream.CanWrite; } }
        public override bool CanSeek { get { return mStream.CanSeek; } }
        public override long Length { get { return mStream.Length; } }
        public override int ReadTimeout {
            get { return mStream.ReadTimeout; }
            set { mStream.ReadTimeout = value; }
        }
        public override int WriteTimeout {
            get { return mStream.WriteTimeout; }
            set { mStream.WriteTimeout = value; }
        }

        public override void Flush() { mStream.Flush(); }
        public override void SetLength(long value) { mStream.SetLength(value); }
    }

    public class ToStringOptions {
        public readonly ToStringFlags Flags;
        public readonly SchemeObjectPtrMap<int> Labels;
        public readonly SchemeObjectPtrSet Visited;

        private ToStringOptions(
                ToStringFlags flags, SchemeObjectPtrMap<int> labels, SchemeObjectPtrSet visited) {
            Flags = flags;
            Labels = labels;
            Visited = visited;
        }

        public ToStringOptions(ToStringFlags flags) {
            Flags = flags;
            Labels = new SchemeObjectPtrMap<int>();
            Visited = new SchemeObjectPtrSet();
        }

        public ToStringOptions(ToStringFlags flags, SchemeObjectPtrSet cycles) {
            Flags = flags;
            Visited = new SchemeObjectPtrSet();

            Labels = new SchemeObjectPtrMap<int>();
            int nextLabel = 0;
            foreach (SchemeObject obj in cycles)
                Labels.Add(obj, nextLabel++);
        }

        public bool ShouldBeQuotedButIsnt {
            get {
                return (Flags & (ToStringFlags.ShouldBeQuoted | ToStringFlags.AlreadyQuoted)) ==
                    ToStringFlags.ShouldBeQuoted;
            }
        }

        public bool ContainsFlags(ToStringFlags query) {
            return (Flags & query) != 0;
        }

        public ToStringOptions DuplicateWithFlags(ToStringFlags newFlags) {
            if ((Flags & newFlags) == newFlags)
                return this;
            return new ToStringOptions(Flags | newFlags, Labels, Visited);
        }

        public ToStringOptions DuplicateWithoutFlags(ToStringFlags newFlags) {
            if ((Flags & newFlags) == 0)
                return this;
            return new ToStringOptions(Flags & ~newFlags, Labels, Visited);
        }

        internal ToStringOptions DuplicateHavingVisited(SchemeObject obj) {
            SchemeObjectPtrSet newVisited = Visited.Clone();
            newVisited.Add(obj);
            return new ToStringOptions(Flags, Labels, newVisited);
        }
    }

    [Flags]
    public enum ToStringFlags : byte {
        ShouldBeQuoted = 1,
        AlreadyQuoted = 2,
        RawStringsAndChars = 4,
        PartOfList = 8,
        MJSExprForm = 16,
    }
}