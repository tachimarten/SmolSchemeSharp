// SmolScheme/Datum.cs
//
// Datums, a.k.a. constants, as defined in R⁷RS section 7.1.2.

using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Runtime.Serialization;
using System.Text;

namespace SmolScheme {
    // See R⁷RS § 7.1.2.
    public abstract class SimpleDatum : SchemeObject {
        internal sealed override SExpr ReplaceIdentifiers(Dictionary<string, SExpr> replacements) {
            return this;
        }

        internal sealed override void RecursivelyReplaceDatumLabel(int label, SExpr replacement) { }
    }

    // Vectors and bytevectors.
    [SchemeBridge.TypeTest]
    public sealed class Symbol : SimpleDatum {
        public readonly string Name;

        internal Symbol(string name, IdentifierRepr repr) : base() {
            Name = Identifier.ToMJSForm(name, repr);
        }

        [SchemeBridge("String->Symbol")]
        public static Symbol FromString(string sRepr) {
            return new Symbol(sRepr, IdentifierRepr.SRepr);
        }

        public override bool Eqv(SchemeObject other) {
            return other is Symbol && this.Name == ((Symbol)other).Name;
        }

        public override string StructureToString(ToStringOptions options) {
            if (options.ShouldBeQuotedButIsnt) {
                return "'" + StructureToString(options.DuplicateWithFlags(
                    options.Flags | ToStringFlags.ShouldBeQuoted | ToStringFlags.AlreadyQuoted));
            }

            return Identifier.IdentifierToString(Name, options);
        }

        internal override SExpr Unquote() {
            return new Identifier(this.Name, IdentifierRepr.MJSRepr, Loc);
        }

        public string SRepr {
            [SchemeBridge("Symbol->String")]
            get { return Identifier.SwitchRepr(Name); }
        }

        [SchemeBridge("Symbol=?")]
        public bool AllEqual(Symbol first, params Symbol[] rest) {
            if (rest.Length == 0)
                throw new SchemeException("At least two symbols must be supplied");
            foreach (Symbol other in rest) {
                if (other.Name != first.Name)
                    return false;
            }
            return true;
        }
    }

    [SchemeBridge.TypeTest("Char?")]
    internal class SchemeChar : SimpleDatum {
        public readonly char Value;

        public SchemeChar(char value) : base() {
            Value = value;
        }

        [SchemeBridge("Integer->Char")]
        public SchemeChar(int value) : this((char)value) { }

        public override string StructureToString(ToStringOptions options) {
            if (options.ContainsFlags(ToStringFlags.RawStringsAndChars))
                return "" + Value;
            if (options.ContainsFlags(ToStringFlags.MJSExprForm))
                return "\'" + EscapeCharMJS(Value, '"') + "\'";
            return "#\\" + EscapeCharS(Value);
        }

        internal static string EscapeCharMJS(char ch, char delim) {
            if (ch == delim)
                return "\\" + ch;

            switch (ch) {
                case '\n':
                    return "\\n";
                case '\r':
                    return "\\r";
                case '\t':
                    return "\\t";
                case '\\':
                    return "\\";
            }

            if (Char.IsControl(ch))
                return String.Format("\\x{0:x4}", (int)ch, 16);
            return "" + ch;
        }

        internal static string EscapeCharS(char ch) {
            switch (ch) {
                case '\x07':
                    return "alarm";
                case '\x08':
                    return "backspace";
                case '\x7f':
                    return "delete";
                case '\x1b':
                    return "escape";
                case '\n':
                    return "newline";
                case '\0':
                    return "null";
                case '\r':
                    return "return";
                case ' ':
                    return "space";
                case '\t':
                    return "tab";
            }

            if (Char.IsControl(ch))
                return String.Format("x{0:x}", (int)ch);
            return "" + ch;
        }

        public override bool Eqv(SchemeObject other) {
            return other is SchemeChar && Value == ((SchemeChar)other).Value;
        }

        public override bool TryBridgeTo(Type type, out object outResult) {
            if (type == typeof(char)) {
                outResult = Value;
                return true;
            }
            return base.TryBridgeTo(type, out outResult);
        }

        internal override SExpr Unquote() {
            return this;
        }

        [SchemeBridge("Char->Integer")]
        public int ToInteger() {
            return (int)Value;
        }

        [SchemeBridge("Char=?")]
        public bool AllEqual(char first, params char[] rest) {
            if (rest.Length == 0)
                throw new SchemeException("At least two characters must be supplied");
            foreach (char other in rest) {
                if (other != first)
                    return false;
            }
            return true;
        }
    }

    // 6.7 Strings

    [SchemeBridge.TypeTest("String?")]
    internal class SchemeString : SimpleDatum {
        public string Value;

        public SchemeString(string value) : base() {
            Value = value;
        }

        [SchemeBridge("String")]
        public SchemeString(params char[] chars) : this(new String(chars)) { }

        [SchemeBridge("MakeString")]
        public SchemeString(int length, char ch = '\0') : this(new String(ch, length)) { }

        [SchemeBridge("Utf8->String")]
        public SchemeString(byte[] utf8, int start, int end) :
            this(Encoding.UTF8.GetString(utf8, start, end - start)) { }

        [SchemeBridge("Utf8->String")]
        public SchemeString(byte[] utf8, int start = 0) : this(utf8, start, utf8.Length) { }

        public override string StructureToString(ToStringOptions options) {
            if (options.ContainsFlags(ToStringFlags.RawStringsAndChars))
                return "" + Value;
            return Escape(Value, '"');
        }

        public static string Escape(string str, char delim) {
            StringBuilder builder = new StringBuilder();
            builder.Append(delim);

            foreach (char ch in str) {
                switch (ch) {
                    case '\x07':
                        builder.Append("\\a");
                        break;
                    case '\x08':
                        builder.Append("\\b");
                        break;
                    case '\t':
                        builder.Append("\\t");
                        break;
                    case '\n':
                        builder.Append("\\n");
                        break;
                    case '\x0d':
                        builder.Append("\\r");
                        break;
                    case '\\':
                        builder.Append("\\\\");
                        break;
                    default:
                        if (Char.IsControl(ch))
                            builder.AppendFormat("\\x{0:x};", (int)ch);
                        else if (ch == delim)
                            builder.AppendFormat("\\{0}", delim);
                        else
                            builder.Append(ch);
                        break;
                }
            }

            builder.Append(delim);
            return "" + builder;
        }

        public override bool Eqv(SchemeObject other) {
            return this == other;
        }

        internal override bool Equal(SchemeObject otherObj, SchemeObjectPtrSet visited) {
            if (!(otherObj is SchemeString))
                return false;
            return this.Value == ((SchemeString)otherObj).Value;
        }

        public int Length {
            [SchemeBridge("StringLength")]
            get { return Value.Length; }
        }

        public char[] ToCharArray() {
            return Value.ToCharArray();
        }

        public override bool TryBridgeTo(Type type, out object outResult) {
            if (type == typeof(string)) {
                outResult = Value;
                return true;
            }
            return base.TryBridgeTo(type, out outResult);
        }

        internal override SExpr Unquote() {
            return this;
        }

        [SchemeBridge("StringAppend")]
        public static string Append(params string[] strings) {
            return String.Join("", strings);
        }

        [SchemeBridge("StringCopy")]
        public string Copy(int start, int end) {
            return Value.Substring(start, end - start);
        }

        [SchemeBridge("StringCopy")]
        public string Copy(int start = 0) {
            return Copy(start, Length);
        }

        [SchemeBridge]
        public string Substring(int start, int end) {
            return Copy(start, end);
        }

        [SchemeBridge]
        public string Substring(int start = 0) {
            return Copy(start);
        }

        [SchemeBridge("String->Utf8")]
        public byte[] ToUTF8(int start, int end) {
            return Encoding.UTF8.GetBytes(Value.Substring(start, end - start));
        }

        [SchemeBridge("String->Utf8")]
        public byte[] ToUTF8(int start = 0) {
            return ToUTF8(start, Length);
        }

        [SchemeBridge("String->List")]
        public SchemeObject ToList(int start, int end) {
            string str = Value.Substring(start, end);
            return SchemeList.CreateList(str.Select(ch => new SchemeChar(ch)));
        }

        [SchemeBridge("Scheme->List")]
        public SchemeObject ToList(int start = 0) {
            return ToList(start, Length);
        }

        public char this[int i] {
            [SchemeBridge("StringRef")]
            get { return Value[i]; }

            [SchemeBridge("StringSet!")]
            set {
                char[] chars = ToCharArray();
                chars[i] = value;
                Value = new String(chars);
            }
        }

        public delegate void ForEachProc(params char[] chars);

        [SchemeBridge("StringForEach")]
        public static void ForEach(ForEachProc proc, string first, params string[] rest) {
            IEnumerable<string> strings = (new string[1] { first }).Concat(rest);
            int length = strings.Select(str => str.Length).Min();
            // FIXME: Is this quadratic?
            char[] chars = new char[strings.Count()];
            for (int i = 0; i < length; i++) {
                for (int j = 0; j < strings.Count(); j++)
                    chars[j] = strings.ElementAt(j)[i];
                proc(chars);
            }
        }

        public delegate char MapProc(params char[] chars);

        [SchemeBridge("StringMap")]
        public static string Map(MapProc proc, string first, params string[] rest) {
            List<char> newChars = new List<char>();
            ForEach(chars => newChars.Add(proc(chars)), first, rest);
            return new String(newChars.ToArray());
        }

        [SchemeBridge("String->Vector")]
        public char[] ToCharArray(int start, int end) {
            return Value.ToCharArray(start, end - start);
        }

        [SchemeBridge("String->Vector")]
        public char[] ToCharArray(int start = 0) {
            return ToCharArray(start, Length);
        }

        [SchemeBridge("StringCopy!")]
        public void Copy(int at, string from, int start, int end) {
            // NB: This handles overlapping arrays properly.
            char[] toChars = Value.ToCharArray(), fromChars = from.ToCharArray();
            Array.Copy(fromChars, start, toChars, at, end - start);
            Value = new String(toChars);
        }

        [SchemeBridge("StringCopy!")]
        public void Copy(int at, string from, int start = 0) {
            Copy(at, from, start, from.Length);
        }

        [SchemeBridge("StringFill!")]
        public void Fill(char fill, int start, int end) {
            char[] chars = Value.ToCharArray();
            for (int i = start; i < end; i++)
                chars[i] = fill;
            Value = new String(chars);
        }

        [SchemeBridge("StringFill!")]
        public void Fill(char fill, int start = 0) {
            Fill(fill, start, Length);
        }
    }

    [SchemeBridge.TypeTest("Boolean?")]
    internal class SchemeBool : SimpleDatum {
        public readonly bool Value;

        private SchemeBool(bool value) : base() {
            Value = value;
        }

        public static readonly SchemeBool True = new SchemeBool(true);
        public static readonly SchemeBool False = new SchemeBool(false);

        public static SchemeBool Create(bool Value) {
            return Value ? True : False;
        }

        public override string StructureToString(ToStringOptions options) {
            if (!options.ContainsFlags(ToStringFlags.MJSExprForm))
                return Value ? "#t" : "#f";
            return Value ? "True" : "False";
        }

        public override bool Eqv(SchemeObject other) {
            return other is SchemeBool && Value == ((SchemeBool)other).Value;
        }

        public override bool IsTruthy {
            get { return Value; }
        }

        public override bool TryBridgeTo(Type type, out object outResult) {
            if (type == typeof(bool)) {
                outResult = Value;
                return true;
            }
            return base.TryBridgeTo(type, out outResult);
        }

        internal override SExpr Unquote() {
            return this;
        }

        [SchemeBridge("Boolean=?")]
        public bool AllEqual(bool first, params bool[] rest) {
            if (rest.Length == 0)
                throw new SchemeException("At least two booleans must be supplied");
            foreach (bool other in rest) {
                if (other != first)
                    return false;
            }
            return true;
        }
    }

    [SchemeBridge.TypeTest]
    public class Vector : SchemeObject {
        public readonly SchemeObject[] mElements;

        public SchemeObject[] Elements {
            get { return mElements; }
        }

        public int Length {
            [SchemeBridge("VectorLength")]
            get { return mElements.Length; }
        }

        [SchemeBridge("Vector")]
        public Vector(SchemeObject[] elements) : base() {
            mElements = elements;
        }

        [SchemeBridge("MakeVector")]
        public Vector(int k, SchemeObject fill = null) {
            if (fill == null)
                fill = SchemeNull.Null;

            mElements = new SchemeObject[k];
            for (int i = 0; i < k; i++)
                mElements[i] = fill;
        }

        public override string StructureToString(ToStringOptions options) {
            string[] elemStrings = Elements.Select(elem => elem.ToString(options)).ToArray();
            if (!options.ContainsFlags(ToStringFlags.MJSExprForm))
                return "#(" + String.Join(" ", elemStrings) + ")";
            return "[" + String.Join(", ", elemStrings) + "]";
        }

        public SchemeObject this[int i] {
            [SchemeBridge("VectorRef")]
            get { return Elements[i]; }
            [SchemeBridge("VectorSet!")]
            set { Elements[i] = value; }
        }

        public override bool Eqv(SchemeObject other) {
            return this == other;
        }

        internal override SExpr Unquote() {
            return this;
        }

        internal override bool Equal(SchemeObject otherObj, SchemeObjectPtrSet visited) {
            if (this == otherObj)
                return true;

            if (visited.Contains(this))
                return false;
            visited.Add(this);

            if (!(otherObj is Vector))
                return false;

            var other = (Vector)otherObj;
            if (Length != other.Length)
                return false;

            for (int i = 0; i < Length; i++) {
                if (!this[i].Equal(other[i], visited))
                    return false;
            }

            return true;
        }

        /// <summary>
        /// Vectors bridge to arrays.
        /// </summary>
        public override bool TryBridgeTo(Type type, out object outResult) {
            if (!type.IsArray)
                return base.TryBridgeTo(type, out outResult);

            Type elementType = type.GetElementType();
            Array array = Array.CreateInstance(elementType, Length);
            for (int i = 0; i < Length; i++) {
                object element = null;
                if (!this[i].TryBridgeTo(elementType, out element))
                    return base.TryBridgeTo(type, out outResult);
                array.SetValue(element, i);
            }

            outResult = array;
            return true;
        }

        [SchemeBridge("VectorCopy")]
        public Vector Copy(int start, int end) {
            return new Vector(Elements.Skip(start).Take(end - start).ToArray());
        }

        [SchemeBridge("VectorCopy")]
        public Vector Copy(int start = 0) {
            return Copy(start, Length);
        }

        [SchemeBridge("VectorCopy!")]
        public void Copy(int at, Vector from, int start, int end) {
            // NB: This handles overlapping arrays properly.
            Array.Copy(from.Elements, start, Elements, at, end - start);
        }

        [SchemeBridge("VectorCopy!")]
        public void Copy(int at, Vector from, int start = 0) {
            Copy(at, from, start, from.Length);
        }

        [SchemeBridge("VectorAppend")]
        public static Vector Append(params Vector[] vectors) {
            return new Vector(vectors.SelectMany(vector => vector.Elements).ToArray());
        }

        [SchemeBridge("Vector->List")]
        public SchemeObject ToList() {
            return SchemeList.CreateList(mElements);
        }

        [SchemeBridge("VectorFill!")]
        public void Fill(SchemeObject fill, int start, int end) {
            for (int i = start; i < end; i++)
                mElements[i] = fill;
        }

        [SchemeBridge("VectorFill!")]
        public void Fill(SchemeObject fill, int start = 0) {
            Fill(fill, start, Length);
        }

        public delegate void ForEachProc(params SchemeObject[] elements);

        [SchemeBridge("VectorForEach")]
        public static void ForEach(ForEachProc proc, Vector first, params Vector[] rest) {
            IEnumerable<Vector> vectors = (new Vector[1] { first }).Concat(rest);
            int length = vectors.Select(vector => vector.Length).Min();

            SchemeObject[] objects = new SchemeObject[vectors.Count()];
            for (int i = 0; i < length; i++) {
                for (int j = 0; j < vectors.Count(); j++)
                    objects[j] = vectors.ElementAt(j)[i];
                proc(objects);
            }
        }

        public delegate SchemeObject MapProc(params SchemeObject[] elements);

        [SchemeBridge("VectorMap")]
        public static SchemeObject Map(MapProc proc, Vector first, params Vector[] rest) {
            List<SchemeObject> newElements = new List<SchemeObject>();
            ForEach(elements => newElements.Add(proc(elements)), first, rest);
            return new Vector(newElements.ToArray());
        }

        [SchemeBridge("Vector->String")]
        public string CharVectorToString(int start, int end) {
            char[] chars = new char[end - start];
            for (int i = 0; i < end - start; i++)
                chars[i] = this[start + i].BridgeTo<char>();
            return new String(chars);
        }

        [SchemeBridge("Vector->String")]
        public string CharVectorToString(int start = 0) {
            return CharVectorToString(start, Length);
        }

        internal override void FindCycles(SchemeObjectPtrSet visited, SchemeObjectPtrSet cycles) {
            if (MarkVisited(visited, cycles)) {
                foreach (SchemeObject element in Elements)
                    element.FindCycles(visited, cycles);
            }
        }
    }

    [SchemeBridge.TypeTest]
    public class Bytevector : SimpleDatum {
        private readonly byte[] mElements;

        public byte[] Elements {
            get { return mElements; }
        }

        public int Length {
            [SchemeBridge("BytevectorLength")]
            get { return mElements.Length; }
        }

        [SchemeBridge]
        public Bytevector(params byte[] data) : base() {
            mElements = data;
        }

        [SchemeBridge("MakeBytevector")]
        public Bytevector(int length, byte fill = 0) {
            mElements = new byte[length];
            for (int i = 0; i < length; i++)
                mElements[i] = fill;
        }

        public override string StructureToString(ToStringOptions options) {
            if (!options.ContainsFlags(ToStringFlags.MJSExprForm))
                return "#u8(" + String.Join(" ", Elements.Select(b => "" + b)) + ")";
            return "Bytevector[" + String.Join(", ", Elements.Select(b => "" + b)) + "]";
        }

        public override bool Eqv(SchemeObject other) {
            return this == other;
        }

        internal override SExpr Unquote() {
            return this;
        }

        public byte this[int i] {
            [SchemeBridge("BytevectorU8Ref")]
            get { return mElements[i]; }
            [SchemeBridge("BytevectorU8Set!")]
            set { mElements[i] = value; }
        }

        internal override bool Equal(SchemeObject otherObj, SchemeObjectPtrSet visited) {
            if (this == otherObj)
                return true;

            if (visited.Contains(this))
                return false;
            visited.Add(this);

            if (!(otherObj is Bytevector))
                return false;

            var other = (Bytevector)otherObj;
            if (Length != other.Length)
                return false;

            for (int i = 0; i < Length; i++) {
                if (this[i] != other[i])
                    return false;
            }

            return true;
        }

        [SchemeBridge("BytevectorCopy")]
        public Bytevector Copy(int start, int end) {
            return new Bytevector(Elements.Skip(start).Take(end - start).ToArray());
        }

        [SchemeBridge("BytevectorCopy")]
        public Bytevector Copy(int start = 0) {
            return Copy(start, Length);
        }

        [SchemeBridge("BytevectorCopy!")]
        public void Copy(int at, Bytevector from, int start, int end) {
            // NB: This handles overlapping arrays properly.
            Array.Copy(from.Elements, start, Elements, at, end - start);
        }

        [SchemeBridge("BytevectorCopy!")]
        public void Copy(int at, Bytevector from, int start = 0) {
            Copy(at, from, start, from.Length);
        }

        [SchemeBridge("BytevectorAppend")]
        public static Bytevector Append(params Bytevector[] vectors) {
            return new Bytevector(vectors.SelectMany(vector => vector.Elements).ToArray());
        }
    }

    /// <summary>
    /// These only exist transiently during parsing.
    /// </summary>
    internal sealed class LabelRefDatum : SimpleDatum {
        public readonly int Label;

        public LabelRefDatum(int label) {
            Label = label;
        }

        public override bool Eqv(SchemeObject other) {
            return other is LabelRefDatum && this.Label == ((LabelRefDatum)other).Label;
        }

        public override string StructureToString(ToStringOptions options) {
            return String.Format("#{0}#", Label);
        }
    }
}