// SmolScheme/Lexer.cs
//
// A lexer for both S-expressions and MJS-expressions, SmolScheme#'s own variant syntax.

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Text;

namespace SmolScheme {
    // Base class for all lexers.
    internal abstract class Lexer : IDisposable {
        private TextReader mReader;
        private int[] mLookahead;

        private string mPath;
        private int mLineNumber;

        protected bool mFoldCase;

        protected abstract bool CharsAreQuoted { get; }

        protected abstract char LineCommentChar { get; }

        protected abstract CharToken ParseChar();

        protected abstract IdentifierToken ParseIdentifier(bool allowDot);

        protected abstract PunctuationToken ParsePunctuation();

        protected abstract Token ParseHashToken();

        protected abstract NumberToken ParseZeroPrefixedNumber();

        public Lexer(TextReader reader, string path = null) {
            // If the path wasn't supplied, see if we can get it from the reader.
            if (path == null && reader is StreamReader) {
                Stream stream = ((StreamReader)reader).BaseStream;
                if (stream is FileStream)
                    path = ((FileStream)stream).Name;
            }

            mReader = reader;
            mPath = path;
            mLineNumber = 1;

            mFoldCase = false;

            mLookahead = new int[2];
            mLookahead[0] = mReader.Read();
            mLookahead[1] = mReader.Read();
        }

        protected int NextChar() {
            int ch = mLookahead[0];
            mLookahead[0] = mLookahead[1];
            mLookahead[1] = mReader.Read();
            return ch;
        }

        protected int PeekChar(int n = 0) {
            return mLookahead[n];
        }

        private void ReadLine() {
            int ch;
            do {
                ch = NextChar();
            } while (ch >= 0 && ch != (int)'\n');
        }

        private bool ConsumeWhitespace() {
            char ch = (char)PeekChar();
            if (ch == '\t' || ch == ' ' || ch == '\r') {
                NextChar();
                return true;
            }
            if (ch == '\n') {
                NextChar();
                mLineNumber++;
                return true;
            }
            return false;
        }

        private StringToken ParseString() {
            char ch = (char)PeekChar();

            if (ch != '"')
                return null;

            StringBuilder output = new StringBuilder();
            NextChar();
            while ((ch = (char)PeekChar()) != 0xffff && ch != '"') {
                if (ch == '\\') {
                    NextChar();
                    if ((ch = (char)PeekChar()) == -1)
                        DidntFind("a string escape");
                    if (ch != '\n')
                        output.Append(TranslateEscape((char)NextChar()));
                } else {
                    output.Append((char)NextChar());
                }
            }

            if (ch == -1)
                DidntFind("`\"`");
            NextChar();

            return new StringToken("" + output, CurrentLoc);
        }

        protected char TranslateEscape(char escape) {
            switch (escape) {
                case 'n':
                    return '\n';
                case 'r':
                    return '\r';
                case 't':
                    return '\t';
                case 'a':
                    return '\x07';
                case 'b':
                    return '\x08';
                case '\\':
                    return '\\';
                case '\'':
                    return '\'';
                case '"':
                    return '"';
                case '|':
                    return '|';
                default:
                    DidntFind("any of `n`, `r`, `t`, `a`, `b`, `\\`, `'`, `\"`, or `|`");
                    return '\0';    // unreachable
            }
        }

        internal NumberToken ParseNumber(int radix, bool? exact) {
            char ch = (char)PeekChar();

            if (!Char.IsDigit(ch) && ch != '.' && (radix != 16 || !IsHex(ch)))
                return null;

            StringBuilder builder = new StringBuilder();
            bool isFloatingPoint = false;

            while ((ch = (char)PeekChar()) != -1 &&
                    (Char.IsDigit(ch) ||
                    (radix == 16 && IsHex(ch)) ||
                    (!isFloatingPoint && ch == '.'))) {
                if (ch == '.')
                    isFloatingPoint = true;
                builder.Append((char)NextChar());
            }

            string token = "" + builder;

            if (radix != 10) {
                if (isFloatingPoint)
                    SyntaxError("Floating point numbers must be specified in decimal");
                return new IntToken(SchemeInt.ParseBigInt(token, radix),
                    exact == null || (bool)exact, CurrentLoc);
            }

            return new DoubleToken(Double.Parse(token), exact != null && (bool)exact, CurrentLoc);
        }

        private static bool IsHex(char ch) {
            return "ABCDEFabcdef".Contains(ch);
        }

        private IdentifierToken ParseVerbatimIdentifier(char delimiter) {
            char ch = (char)PeekChar();

            if (ch != delimiter)
                return null;

            StringBuilder token = new StringBuilder();
            NextChar();

            while ((ch = (char)PeekChar()) != delimiter && ch != -1) {
                if (ch == '\\') {
                    NextChar();
                    token.Append(TranslateEscape((char)NextChar()));
                } else {
                    token.Append((char)NextChar());
                }
            }

            if (ch == -1)
                DidntFind(String.Format("`{0}`", delimiter));
            NextChar();

            return new IdentifierToken("" + token, mFoldCase, CurrentLoc);
        }

        public void Dispose() {
            mReader.Dispose();
        }

        protected SourceLocation CurrentLoc {
            get {
                return new SourceLocation(mPath, mLineNumber);
            }
        }

        public Token Next() {
            Token token;

            char ch;
            while ((ch = (char)PeekChar()) != 0xffff) {
                // Whitespace
                if (ConsumeWhitespace())
                    continue;

                // Comments
                if (ch == LineCommentChar) {
                    ReadLine();
                    mLineNumber++;
                    continue;
                }

                // Identifiers
                if ((token = ParseIdentifier(true)) != null) {
                    // Special case.
                    if (((IdentifierToken)token).Symbol == ".")
                        return new PunctuationToken(".", token.Loc);
                    return token;
                }

                // Verbatim identifiers
                if ((token = ParseVerbatimIdentifier('`')) != null)
                    return token;

                // Strings
                if ((token = ParseString()) != null)
                    return token;

                // Zero-prefixed numbers
                if (ch == '0' && (token = ParseZeroPrefixedNumber()) != null)
                    return token;

                // Dot-prefixed numbers, or ellipsis
                if (ch == '.' && (token = ParseDotPrefixedToken()) != null)
                    return token;

                // Plain numbers
                if ((token = ParseNumber(10, null)) != null)
                    return token;

                // Single quote
                if (ch == '\'') {
                    NextChar();

                    if (CharsAreQuoted && (token = ParseChar()) != null)
                        return token;

                    return new PunctuationToken("'", CurrentLoc);
                }

                // Anything beginning with `#` (for S-expressions)
                if ((token = ParseHashToken()) != null)
                    return token;

                // Punctuation
                if ((token = ParsePunctuation()) != null)
                    return token;

                DidntFind("valid character");
            }

            return new EofToken(CurrentLoc);
        }

        private Token ParseDotPrefixedToken() {
            NextChar();

            char ch = (char)PeekChar();
            if (ch == '.') {
                NextChar();
                if (NextChar() != '.')
                    SyntaxError("\"..\" isn't valid; write \"...\" instead");
                return new PunctuationToken("...", CurrentLoc);
            }

            StringBuilder token = new StringBuilder(".");
            if (Char.IsDigit(ch)) {
                do {
                    token.Append(ch);
                } while (Char.IsDigit(ch = (char)PeekChar()));
                return new DoubleToken(Double.Parse("" + token), false, CurrentLoc);
            }

            return new PunctuationToken(".", CurrentLoc);
        }

        protected void SyntaxError(string error) {
            throw new ParseException("Syntax error: " + error, CurrentLoc);
        }

        protected void DidntFind(string expected, string actual) {
            SyntaxError(String.Format("Expected {0} but found {1}", expected, actual));
        }

        protected void DidntFind(string expected) {
            char ch = (char)PeekChar();
            DidntFind(expected, ch == 0xffff ? "end of input" : String.Format("`{0}`", (char)ch));
        }
    }

    // S-expression lexer
    internal sealed class SLexer : Lexer, IDisposable {
        public SLexer(TextReader reader, string path = null) : base(reader, path) { }

        protected override char LineCommentChar {
            get { return ';'; }
        }

        protected override bool CharsAreQuoted {
            get { return false; }
        }

        private static readonly Dictionary<string, char> mCharacterNames =
                new Dictionary<string, char>() {
            { "alarm", '\x07' },
            { "backspace", '\x08' },
            { "delete", '\x7f' },
            { "escape", '\x1b' },
            { "newline", '\n' },
            { "null", '\0' },
            { "return", '\r' },
            { "space", ' ' },
            { "tab", '\t' },
        };

        protected override CharToken ParseChar() {
            // Check for an escaped quoted character.
            char literal;
            if (PeekChar() == (int)'\\') {
                NextChar();
                literal = TranslateEscape((char)NextChar());
                return new CharToken(literal, CurrentLoc);
            }

            // Check for a quoted character.
            literal = (char)NextChar();
            return new CharToken(literal, CurrentLoc);
        }

        protected override IdentifierToken ParseIdentifier(bool allowDot = false) {
            char ch = (char)PeekChar();

            // Verbatim identifier syntax.
            if (ch == '|') {
                NextChar();
                return ParseVerbatimIdentifier();
            }

            if (!IsNonDigitIdentifierChar(ch))
                return null;

            StringBuilder token = new StringBuilder();
            while (true) {
                ch = (char)PeekChar();
                if (!IsNonDigitIdentifierChar(ch) && !Char.IsDigit(ch))
                    break;
                NextChar();
                token.Append(ch);
            }

            string identifier = "" + token;
            if (!allowDot && identifier == ".")
                SyntaxError("`.` is not an identifier");

            return new IdentifierToken(identifier, mFoldCase, CurrentLoc);
        }

        private IdentifierToken ParseVerbatimIdentifier() {
            StringBuilder token = new StringBuilder();

            char ch;
            while ((ch = (char)PeekChar()) != '|') {
                if (ch == '\\') {
                    NextChar();
                    token.Append(TranslateEscape((char)NextChar()));
                } else {
                    token.Append((char)NextChar());
                }
            }

            NextChar();
            return new IdentifierToken("" + token, mFoldCase, CurrentLoc);
        }

        protected override Token ParseHashToken() {
            if ((char)PeekChar() != '#')
                return null;
            NextChar();

            IdentifierToken identifier;
            char ch = (char)PeekChar();
            switch (ch) {
                case ';':
                    // "Comment out" syntax.
                    NextChar();
                    return new PunctuationToken("#;", CurrentLoc);

                case '|':
                    // Block comment syntax. Parse it and go around the loop again.
                    NextChar();
                    ConsumeBlockComment();
                    return Next();

                case 't':
                case 'f':
                    identifier = ParseIdentifier();
                    if (identifier.Symbol == "t" || identifier.Symbol == "true")
                        return new BoolToken(true, identifier.Loc);
                    if (identifier.Symbol == "f" || identifier.Symbol == "false")
                        return new BoolToken(false, identifier.Loc);
                    DidntFind("`t`, `true`, `f`, or `false`", identifier.Symbol);
                    return null;    // unreachable

                case 'u':
                    identifier = ParseIdentifier();
                    if (identifier.Symbol != "u8")
                        DidntFind("`u8`", identifier.Symbol);
                    if ((char)PeekChar() != '(')
                        DidntFind("`(`");
                    NextChar();
                    return new PunctuationToken("#u8(", CurrentLoc);

                case '(':
                    NextChar();
                    return new PunctuationToken("#(", CurrentLoc);

                case '\\':
                    // Handle character literals.
                    NextChar();
                    ch = (char)PeekChar();
                    if (ch == 0xffff)
                        DidntFind("character after `#\\`");

                    if (Char.IsLetter(ch)) {
                        identifier = ParseIdentifier();
                        if (mCharacterNames.ContainsKey(identifier.Symbol)) {
                            return new CharToken(
                                mCharacterNames[identifier.Symbol], identifier.Loc);
                        }
                        if (identifier.Symbol.StartsWith("x")) {
                            int hexValue = Convert.ToInt32(identifier.Symbol.TrimStart('x'), 16);
                            return new CharToken((char)hexValue, identifier.Loc);
                        }
                    }

                    NextChar();
                    return new CharToken(ch, CurrentLoc);

                case 'e':
                case 'i':
                case 'b':
                case 'o':
                case 'd':
                case 'x':
                    // Handle prefixed numbers.
                    return ParsePrefixedNumber();

                case '!':
                    // A directive. Parse it and go around the loop again.
                    NextChar();
                    identifier = ParseIdentifier();
                    ParseDirective(identifier);
                    return Next();

                default:
                    DatumLiteralToken token = ParseDatumLiteralToken();
                    if (token != null)
                        return token;

                    return new PunctuationToken("#", CurrentLoc);
            }
        }

        internal static bool IsNonDigitIdentifierChar(char ch) {
            return Char.IsLetter(ch) || "!$%&*+-./:<=>?@^_~".Contains("" + ch);
        }

        internal NumberToken ParsePrefixedNumber(int radix = 10) {
            bool radixSpecified = false;
            bool? exact = null;

            while (true) {
                char ch = (char)PeekChar();
                switch (ch) {
                    case 'e':
                    case 'i':
                        NextChar();
                        if (exact != null)
                            SyntaxError("Only one of `#e` or `#i` may be specified");

                        exact = ch == 'e';
                        break;

                    case 'b':
                    case 'o':
                    case 'd':
                    case 'x':
                        NextChar();
                        if (radixSpecified)
                            SyntaxError("Only one of `#b`, `#o`, `#d`, or `#x` may be specified");
                        switch (ch) {
                            case 'b': radix = 2; break;
                            case 'o': radix = 8; break;
                            case 'd': radix = 10; break;
                            case 'x': radix = 16; break;
                        }
                        radixSpecified = true;
                        break;

                    default:
                        DidntFind("`#e`, `#i`, `#b`, `#o`, `#d`, or `#x`");
                        break;  // unreachable
                }

                // Go around again if we have another `#`.
                if (PeekChar() == '#') {
                    NextChar();
                    continue;
                }

                break;
            }

            return ParseNumber(radix, exact);
        }

        /// <summary>
        /// Parses a `#| ... |#` block comment.
        /// </summary>
        private void ConsumeBlockComment() {
            char ch;
            while ((ch = (char)PeekChar()) != 0xffff) {
                // Comments nest.
                if (ch == '#') {
                    NextChar();
                    if ((ch = (char)PeekChar()) == '|') {
                        NextChar();
                        ConsumeBlockComment();
                    }
                    continue;
                }

                if (ch == '|') {
                    NextChar();
                    if ((ch = (char)PeekChar()) == '#') {
                        NextChar();
                        return;
                    }
                    continue;
                }

                NextChar();
            }
        }

        /// <summary>
        /// Handles datum literals.
        /// </summary>
        private DatumLiteralToken ParseDatumLiteralToken() {
            char ch = (char)PeekChar();
            if (!Char.IsDigit(ch))
                return null;

            StringBuilder datumString = new StringBuilder();
            do {
                datumString.Append(ch);
                NextChar();
                ch = (char)PeekChar();
            } while (Char.IsDigit(ch));

            int label = Convert.ToInt32("" + datumString);

            switch (ch) {
                case '#':
                    return new DatumLiteralRefToken(label, CurrentLoc);
                case '=':
                    return new DatumLiteralLabelToken(label, CurrentLoc);
            }

            DidntFind("`#` or `=`");
            return null;    // unreachable
        }

        protected override PunctuationToken ParsePunctuation() {
            char ch = (char)PeekChar();
            if (!"().".Contains("" + ch))
                return null;

            StringBuilder punct = new StringBuilder();
            punct.Append((char)NextChar());
            return new PunctuationToken("" + punct, CurrentLoc);
        }

        protected override NumberToken ParseZeroPrefixedNumber() {
            return null;
        }

        private void ParseDirective(IdentifierToken identifier) {
            switch (identifier.Symbol) {
                case "FoldCase":
                    mFoldCase = true;
                    break;
                case "NoFoldCase":
                    mFoldCase = false;
                    break;
                default:
                    DidntFind("`FoldCase` or `NoFoldCase`");
                    break;
            }
        }
    }

    // MJS-expression lexer
    internal sealed class MJSLexer : Lexer, IDisposable {
        public MJSLexer(TextReader reader, string path = null) : base(reader, path) { }

        protected override bool CharsAreQuoted {
            get { return true; }
        }

        protected override char LineCommentChar {
            get { return '#'; }
        }

        protected override CharToken ParseChar() {
            // Check for an escaped quoted character.
            if (PeekChar() == (int)'\\') {
                NextChar();
                char literal = TranslateEscape((char)NextChar());
                Expect('\'');
                return new CharToken(literal, CurrentLoc);
            }

            // Check for a quoted character.
            if (PeekChar(1) == (int)'\'') {
                char literal = (char)NextChar();
                Expect('\'');
                return new CharToken(literal, CurrentLoc);
            }

            return null;
        }

        protected override PunctuationToken ParsePunctuation() {
            char ch = (char)PeekChar();

            if (!"[](){},+-*/&|=<>.;%:!".Contains("" + ch))
                return null;

            string punct = "" + (char)NextChar();
            char nextCh = (char)PeekChar();
            if (nextCh == -1)
                return new PunctuationToken(punct, CurrentLoc);

            if ("<>:".Contains(punct)) {
                if (nextCh == '=')
                    punct += (char)NextChar();
            } else if ("&|".Contains(punct)) {
                if (nextCh == ch)
                    punct += (char)NextChar();
            } else if (punct == "=") {
                if (nextCh == '>') {
                    punct += (char)NextChar();
                } else {
                    if (nextCh == '=')
                        punct += (char)NextChar();

                    nextCh = (char)PeekChar();
                    if (nextCh == '=')
                        punct += (char)NextChar();
                }
            }

            return new PunctuationToken(punct, CurrentLoc);
        }

        protected override IdentifierToken ParseIdentifier(bool allowDot) {
            char ch = (char)PeekChar();
            if (!IsFirstIdentifierChar(ch))
                return null;

            StringBuilder token = new StringBuilder();
            while ((ch = (char)PeekChar()) != -1 && IsIdentifierChar(ch))
                token.Append((char)NextChar());
            return new IdentifierToken("" + token, mFoldCase, CurrentLoc);
        }

        public static bool IsFirstIdentifierChar(char ch) {
            return Char.IsLetter(ch) || ch == '_';
        }

        public static bool IsIdentifierChar(char ch) {
            return Char.IsLetterOrDigit(ch) || "_?!".Contains(ch);
        }

        protected override Token ParseHashToken() {
            // `#` is for comments, so there's nothing to do here.
            return null;
        }

        private void Expect(char expected) {
            if ((char)PeekChar() != expected)
                DidntFind(String.Format("`{0}`", expected));
            NextChar();
        }

        protected override NumberToken ParseZeroPrefixedNumber() {
            NextChar();

            char ch = (char)PeekChar();
            if (ch == 'x' || ch == 'X') {
                NextChar();
                return ParseNumber(16, null);
            }
            if (Char.IsDigit(ch))
                return ParseNumber(8, null);
            if (ch == '.')
                return ParseNumber(10, null);
            return new IntToken(0, true, CurrentLoc);
        }
    }

    // Tokens

    internal abstract class Token {
        internal readonly SourceLocation Loc;

        protected Token(SourceLocation loc) {
            Loc = loc;
        }
    }

    internal class PunctuationToken : Token {
        public readonly string Punctuation;

        public PunctuationToken(string s, SourceLocation loc) : base(loc) {
            Punctuation = s;
        }

        public override string ToString() {
            return Punctuation;
        }
    }

    internal class IdentifierToken : Token {
        public readonly string Symbol;
        public readonly bool CaseFolding;

        public IdentifierToken(string symbol, bool caseFolding, SourceLocation loc) : base(loc) {
            Symbol = symbol;
            CaseFolding = caseFolding;
        }
    }

    internal class CharToken : Token {
        public readonly char Value;

        public CharToken(char value, SourceLocation loc) : base(loc) {
            Value = value;
        }
    }

    internal class StringToken : Token {
        public readonly string Value;

        public StringToken(string value, SourceLocation loc) : base(loc) {
            Value = value;
        }
    }

    internal abstract class NumberToken : Token {
        public readonly bool Exact;

        public NumberToken(bool exact, SourceLocation loc) : base(loc) {
            Exact = exact;
        }
    }

    internal sealed class IntToken : NumberToken {
        public readonly BigInteger Value;

        public IntToken(BigInteger value, bool exact, SourceLocation loc) : base(exact, loc) {
            Value = value;
        }
    }

    internal sealed class DoubleToken : NumberToken {
        public readonly double Value;

        public DoubleToken(double value, bool exact, SourceLocation loc) : base(exact, loc) {
            Value = value;
        }
    }

    internal sealed class BoolToken : Token {
        public readonly bool Value;

        public BoolToken(bool value, SourceLocation loc) : base(loc) {
            Value = value;
        }
    }

    internal abstract class DatumLiteralToken : Token {
        public int Label;

        public DatumLiteralToken(int label, SourceLocation loc) : base(loc) {
            Label = label;
        }
    }

    internal sealed class DatumLiteralRefToken : DatumLiteralToken {
        public DatumLiteralRefToken(int label, SourceLocation loc) : base(label, loc) { }
    }

    internal sealed class DatumLiteralLabelToken : DatumLiteralToken {
        public DatumLiteralLabelToken(int label, SourceLocation loc) : base(label, loc) { }
    }

    internal sealed class EofToken : Token {
        public EofToken(SourceLocation loc) : base(loc) { }

        public override string ToString() {
            return "end of input";
        }
    }
}