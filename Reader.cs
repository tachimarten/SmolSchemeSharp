// SmolScheme/MJSReader.cs
//
// A reader for both S-expressions and MJS-expressions, SmolScheme#'s own variant syntax.

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;

namespace SmolScheme {
    /// <summary>
    /// Base class for all readers.
    /// </summary>
    internal abstract class Reader : IDisposable {
        private readonly Lexer mLexer;
        private readonly Token[] mLookahead;

        public Reader(Lexer lexer) {
            mLexer = lexer;
            mLookahead = new Token[2];
            mLookahead[0] = mLexer.Next();
            mLookahead[1] = mLexer.Next();
        }

        public void Dispose() {
            mLexer.Dispose();
        }

        /// <summary>
        /// When at top-level (either at a REPL or when evaluating a file), call this function
        /// before calling `ParseSExpr()`. In MJS mode, this consumes `;`s, which are allowed at
        /// top-level only. (Within lists, they instead separate list elements.)
        /// </summary>
        public abstract void AdvanceToNextTopLevelStatement();

        public abstract SExpr ParseSExpr();

        protected Token NextToken() {
            Token token = mLookahead[0];
            mLookahead[0] = mLookahead[1];
            mLookahead[1] = mLexer.Next();
            return token;
        }

        protected bool LookaheadIsPunctuation(string punctuation) {
            return mLookahead[0] != null &&
                mLookahead[0] is PunctuationToken &&
                ((PunctuationToken)mLookahead[0]).Punctuation == punctuation;
        }

        protected bool ConsumePunctuation(string punct) {
            if (!LookaheadIsPunctuation(punct))
                return false;
            NextToken();
            return true;
        }

        protected PunctuationToken GetPunctuation(string punct) {
            if (LookaheadIsPunctuation(punct))
                return (PunctuationToken)NextToken();
            return null;
        }

        protected void ExpectPunctuation(string punct) {
            if (!ConsumePunctuation(punct))
                DidntFind(String.Format("`{0}`", punct));
        }

        protected Token PeekToken(int n = 0) {
            return mLookahead[n];
        }

        /// Parses number, character, and string literals.
        protected SExpr ParseAtomicLiteral() {
            Token token = PeekToken();

            // Numeric values
            if (token is NumberToken) {
                var numberToken = (NumberToken)NextToken();
                return SchemeNumber.Create(numberToken);
            }

            // Character literals
            if (token is CharToken)
                return new SchemeChar(((CharToken)NextToken()).Value);

            // String literals
            if (token is StringToken)
                return new SchemeString(((StringToken)NextToken()).Value);

            return null;
        }

        protected void DidntFind(string production) {
            throw new ParseException(String.Format("Expected {0} but found {1}",
                production, PeekToken()), PeekToken().Loc);
        }

        protected T Expect<T>(T value, string production) {
            if (value == null)
                DidntFind(production);
            return value;
        }

        public void ExpectEof(string whatWasExpected) {
            if (!(PeekToken() is EofToken))
                DidntFind(whatWasExpected);
        }
    }

    /// <summary>
    /// Reader for S-expressions.
    /// </summary>
    internal class SReader : Reader, IDisposable {
        public SReader(SourceStream stream, bool foldCase = false) :
            base(new SLexer(stream, foldCase)) { }

        public SReader(TextReader textReader, string path = null) :
            base(new SLexer(textReader, path)) { }

        public SReader(string path) :
            this(new StreamReader(new FileStream(path, FileMode.Open)), path) { }

        public override void AdvanceToNextTopLevelStatement() { }

        public override SExpr ParseSExpr() {
            Token token = GetPunctuation("(");
            if (token != null)
                return ParseListExpr(token);

            // Vector literals.
            if ((token = GetPunctuation("#(")) != null)
                return ParseVectorExpr(token, false);

            // Atomic literals.
            SExpr expr = ParseAtomicLiteral();
            if (expr != null)
                return expr;

            token = PeekToken();

            // Datum labels.
            if (token is DatumLiteralLabelToken) {
                var labelToken = (DatumLiteralLabelToken)NextToken();
                expr = ParseSExpr();
                expr.RecursivelyReplaceDatumLabel(labelToken.Label, expr);
                return expr;
            }

            // Datum label references.
            if (token is DatumLiteralRefToken)
                return new LabelRefDatum(((DatumLiteralRefToken)token).Label);

            // Bytevector literals.
            if ((token = GetPunctuation("#u8(")) != null) {
                NextToken();
                return ParseVectorExpr(token, true);
            }

            // Identifiers.
            token = PeekToken();
            if (token is IdentifierToken) {
                IdentifierToken identifierToken = (IdentifierToken)NextToken();
                IdentifierRepr repr;
                if (identifierToken.CaseFolding)
                    repr = IdentifierRepr.SReprFoldCase;
                else
                    repr = IdentifierRepr.SRepr;

                return new Identifier(identifierToken.Symbol, repr, token.Loc);
            }

            if (token is BoolToken) {
                BoolToken boolToken = (BoolToken)NextToken();
                return SchemeBool.Create(boolToken.Value);
            }

            // Quotes and quasiquotes.
            if (token is PunctuationToken) {
                var punct = (PunctuationToken)NextToken();

                // Everything after punctuation is an S-expression, so...
                expr = Expect(ParseSExpr(), String.Format("expression after {0}", punct));

                string desugaring;
                switch (punct.Punctuation) {
                    case "#;":
                        // Comment syntax.
                        return ParseSExpr();
                    case "'":
                        desugaring = "Quote";
                        break;
                    case "`":
                        desugaring = "Quasiquote";
                        break;
                    case ",":
                        desugaring = "Unquote";
                        break;
                    case ",@":
                        desugaring = "UnquoteSplicing";
                        break;
                    default:
                        throw new ParseException("Unexpected punctuation: `" + punct.Punctuation +
                            "`", punct.Loc);
                }

                return new ListExpr(new Identifier(desugaring, IdentifierRepr.MJSRepr, punct.Loc),
                    new SExpr[1] { expr });
            }

            return null;
        }

        private SExpr ParseListExpr(Token openingParen) {
            List<SExpr> elements = new List<SExpr>();
            SExpr tail = null;

            while (!ConsumePunctuation(")")) {
                // Parse dot.
                PunctuationToken punct;
                if ((punct = GetPunctuation(".")) != null) {
                    tail = Expect(ParseSExpr(), "pair tail");
                    ExpectPunctuation(")");
                    break;
                }

                elements.Add(Expect(ParseSExpr(), "list element"));
            }

            if (elements.Count == 0)
                return new NullExpr(openingParen.Loc);

            SExpr head = elements.First();
            SExpr[] body = elements.Skip(1).ToArray();
            if (tail == null)
                return new ListExpr(head, body, head.Loc);
            return new SplatExpr(head, body, tail, head.Loc);
        }

        private SExpr ParseVectorExpr(Token openingParen, bool isBytevector) {
            List<SExpr> elements = new List<SExpr>();

            while (!ConsumePunctuation(")")) {
                SExpr element = ParseSExpr();
                if (element == null) {
                    throw new ParseException("Expected vector element but found " + PeekToken(),
                        elements.Count == 0 ? openingParen.Loc : elements.Last().Loc);
                }
                elements.Add(element);
            }

            if (isBytevector)
                return new BytevectorExpr(elements.ToArray(), openingParen.Loc);
            return new VectorExpr(elements.ToArray(), openingParen.Loc);
        }
    }

    /// <summary>
    /// Reader for MJS-expressions, SmolScheme#'s variant syntax.
    /// </summary>
    internal class MJSReader : Reader, IDisposable {
        public MJSReader(SourceStream stream) : base(new MJSLexer(stream)) { }

        public MJSReader(TextReader textReader, string path = null) :
            base(new MJSLexer(textReader, path)) { }

        public MJSReader(string path) :
            this(new StreamReader(new FileStream(path, FileMode.Open)), path) { }

        public override void AdvanceToNextTopLevelStatement() {
            while (ConsumePunctuation(";")) {}
        }

        public override SExpr ParseSExpr() {
            return ParseLambdaExpr();
        }

        private SExpr ParseLambdaExpr() {
            SExpr expr = ParseDefinitionExpr();
            if (expr == null)
                return null;

            if (ConsumePunctuation("=>")) {
                SExpr rhs = Expect(ParseLambdaExpr(), "expression on right-hand side of \"=>\"");

                // `x => x + 1` becomes `Lambda[[x], x + 1]`.
                if (expr is Identifier) {
                    return new ListExpr("Lambda", new SExpr[] {
                        new ListExpr(expr, new SExpr[] {}, expr.Loc),
                        rhs,
                    }, expr.Loc);
                }

                // Otherwise, `[x, y] => x + y` becomes `Lambda[[x, y], x + y]`.
                return new ListExpr("Lambda", new SExpr[] { expr, rhs }, expr.Loc);
            }

            return expr;
        }

        private SExpr ParseDefinitionExpr() {
            SExpr expr = ParseAssignmentExpr();
            if (expr == null)
                return null;

            if (ConsumePunctuation(":="))
                return DesugarBinOp(expr, ParseDefinitionExpr(), "Define");

            return expr;
        }

        private SExpr ParseAssignmentExpr() {
            SExpr expr = ParseLogicalExpr();
            if (expr == null)
                return null;

            if (ConsumePunctuation("="))
                return DesugarBinOp(expr, ParseAssignmentExpr(), "Set!");

            return expr;
        }

        private SExpr ParseLogicalExpr() {
            SExpr expr = ParseRelationalExpr();
            if (expr == null)
                return null;

            if (ConsumePunctuation("&&"))
                return DesugarBinOp(expr, ParseLogicalExpr(), "And");
            if (ConsumePunctuation("||"))
                return DesugarBinOp(expr, ParseLogicalExpr(), "Or");

            return expr;
        }

        private SExpr ParseRelationalExpr() {
            SExpr expr = ParseAdditiveExpr();
            if (expr == null)
                return null;

            if (ConsumePunctuation("<="))
                return DesugarBinOp(expr, ParseRelationalExpr(), "<=");
            if (ConsumePunctuation(">="))
                return DesugarBinOp(expr, ParseRelationalExpr(), ">=");
            if (ConsumePunctuation("<"))
                return DesugarBinOp(expr, ParseRelationalExpr(), "<");
            if (ConsumePunctuation(">"))
                return DesugarBinOp(expr, ParseRelationalExpr(), ">");
            if (ConsumePunctuation("==="))
                return DesugarBinOp(expr, ParseRelationalExpr(), "Eqv?");
            if (ConsumePunctuation("=="))
                return DesugarBinOp(expr, ParseRelationalExpr(), "=");
            if (ConsumePunctuation("!="))
                return DesugarBinOp(expr, ParseRelationalExpr(), "!=");

            return expr;
        }

        private SExpr ParseAdditiveExpr() {
            SExpr expr = ParseMultiplicativeExpr();
            if (expr == null)
                return null;

            if (ConsumePunctuation("+"))
                return DesugarBinOp(expr, ParseAdditiveExpr(), "+");
            if (ConsumePunctuation("-"))
                return DesugarBinOp(expr, ParseAdditiveExpr(), "-");

            return expr;
        }

        private SExpr ParseMultiplicativeExpr() {
            SExpr expr = ParseUnaryExpr();
            if (expr == null)
                return null;

            if (ConsumePunctuation("*"))
                return DesugarBinOp(expr, ParseMultiplicativeExpr(), "*");
            if (ConsumePunctuation("/"))
                return DesugarBinOp(expr, ParseMultiplicativeExpr(), "/");
            if (ConsumePunctuation("%"))
                return DesugarBinOp(expr, ParseMultiplicativeExpr(), "Modulo");

            return expr;
        }

        private SExpr ParseUnaryExpr() {
            PunctuationToken punct;
            if ((punct = GetPunctuation("-")) != null)
                return DesugarUnOp(ParseUnaryExpr(), "-", punct.Loc);
            if ((punct = GetPunctuation("!")) != null)
                return DesugarUnOp(ParseUnaryExpr(), "Not", punct.Loc);

            return ParsePairExpr();
        }

        private SExpr ParsePairExpr() {
            return ParseSuffixedExpr();
        }

        private SExpr ParseSuffixedExpr() {
            SExpr head = ParseHead();
            if (head == null)
                return null;

            while (true) {
                // Parse a list or pair delimited with any brackets.
                if (LookaheadIsPunctuation("[")) {
                    head = ParseListOrPair(head, "]");
                    continue;
                }
                if (LookaheadIsPunctuation("(")) {
                    head = ParseListOrPair(head, ")");
                    continue;
                }
                if (LookaheadIsPunctuation("{")) {
                    head = ParseListOrPair(head, "}");
                    continue;
                }

                // Parse member access.
                if (ConsumePunctuation(".")) {
                    Token token = NextToken();
                    if (!(token is IdentifierToken))
                        DidntFind("symbol after `.`");

                    head = new ListExpr(
                        new Identifier("Ref", IdentifierRepr.MJSRepr),
                        new SExpr[] {
                            head,
                            new Symbol(((IdentifierToken)token).Symbol, IdentifierRepr.MJSRepr),
                        },
                        head.Loc);
                    continue;
                }

                break;
            }

            return head;
        }

        private SExpr ParseListOrPair(SExpr head, string closingDelim) {
            NextToken();

            if (ConsumePunctuation("...")) {
                SExpr tail = Expect(ParseSExpr(), "list tail");
                ExpectPunctuation(closingDelim);
                return new SplatExpr(head, tail, head.Loc);
            }

            SExpr splat = null;
            SExpr[] elements = ParseListElements(
                closingDelim,
                /*colonCreatesPairs=*/false,
                head.Loc,
                out splat).ToArray();
            if (splat == null)
                return new ListExpr(head, elements, head.Loc);
            return new SplatExpr(head, elements, splat, head.Loc);
        }

        private List<SExpr> ParseListElements(
                string closingDelim,
                bool colonCreatesPairs,
                SourceLocation startingLoc,
                out SExpr outSplat) {
            var elements = new List<SExpr>();

            bool first = true;
            while (true) {
                bool foundSeparator = ConsumePunctuation(",") || ConsumePunctuation(";");

                if (first && foundSeparator)
                    DidntFind(String.Format("list element, `{0}`, `'`, or `...`", closingDelim));

                // Handle splat.
                if (ConsumePunctuation("...")) {
                    if (!first && !foundSeparator)
                        DidntFind("`,` or `;`");

                    SExpr tail = Expect(ParseSExpr(), "tail of list");
                    ExpectPunctuation(closingDelim);

                    outSplat = tail;
                    return elements;
                }

                if (ConsumePunctuation(closingDelim)) {
                    outSplat = null;
                    return elements;
                }

                if (!first && !foundSeparator)
                    DidntFind("`,` or `;`");

                first = false;

                // Handle `key: value`.
                if ((PeekToken(0) is IdentifierToken ||
                        (colonCreatesPairs && PeekToken(0) is StringToken)) &&
                        PeekToken(1) is PunctuationToken &&
                        ((PunctuationToken)PeekToken(1)).Punctuation == ":") {
                    SchemeObject keyDatum = null;
                    if (PeekToken(0) is IdentifierToken) {
                        string name = ((IdentifierToken)PeekToken(0)).Symbol;
                        if (colonCreatesPairs)
                            keyDatum = new Symbol(name, IdentifierRepr.MJSRepr);
                        else
                            keyDatum = new Srfi88.Keyword(name);
                    } else if (PeekToken(0) is StringToken) {
                        keyDatum = new SchemeString(((StringToken)PeekToken(0)).Value);
                    }

                    NextToken();
                    NextToken();

                    SExpr valueExpr = Expect(ParseSExpr(), "expression on right-hand side of `:`");

                    if (colonCreatesPairs) {
                        elements.Add(new SplatExpr(keyDatum, valueExpr, keyDatum.Loc));
                    } else {
                        elements.Add(keyDatum);
                        elements.Add(valueExpr);
                    }
                } else {
                    elements.Add(Expect(ParseSExpr(), "list element"));
                }
            }
        }

        private SExpr ParseHead() {
            SExpr expr = ParseAtomicLiteral();
            if (expr != null)
                return expr;

            Token token = NextToken();

            // Identifiers
            if (token is IdentifierToken) {
                return new Identifier(
                    ((IdentifierToken)token).Symbol, IdentifierRepr.MJSRepr, token.Loc);
            }

            // Punctuation
            if (token is PunctuationToken) {
                var punctuationToken = (PunctuationToken)token;
                string punct = punctuationToken.Punctuation;
                if (punct == "'")
                    return ParseHead().Quote();

                // Parentheses.
                if (punct == "(") {
                    expr = Expect(ParseSExpr(), "expression");
                    ExpectPunctuation(")");
                    return expr;
                }

                // Raw list syntax, with `{}`.
                if (punct == "{") {
                    SExpr splat = null;
                    List<SExpr> elements = ParseListElements(
                        "}",
                        /*colonCreatesPairs=*/true,
                        punctuationToken.Loc,
                        out splat);

                    if (elements.Count == 0) {
                        Debug.Assert(splat == null);
                        return SchemeNull.Null;
                    }

                    SExpr head = elements[0];
                    elements.RemoveAt(0);
                    if (splat != null)
                        return new SplatExpr(head, elements.ToArray(), splat, head.Loc);
                    return new ListExpr(head, elements.ToArray(), head.Loc);
                }

                // Vectors, with `[]`.
                if (punct == "[") {
                    SExpr splat = null;
                    List<SExpr> elements = ParseListElements(
                        "]",
                        /*colonCreatesPairs=*/false,
                        punctuationToken.Loc,
                        out splat);
                    return new VectorExpr(elements.ToArray(), token.Loc);
                }

                throw new ParseException("Unexpected punctuation: `" + punct + "`", token.Loc);
            }

            return null;
        }

        private SExpr DesugarBinOp(SExpr lhs, SExpr rhs, string opName) {
            Expect(rhs, String.Format("expression on right-hand side of `{0}`", opName));
            return new ListExpr(opName, new SExpr[] { lhs, rhs }, lhs.Loc);
        }

        private SExpr DesugarUnOp(SExpr rhs, string opName, SourceLocation loc) {
            Expect(rhs, String.Format("expression after `{0}`", opName));
            return new ListExpr(opName, new SExpr[] { rhs }, rhs.Loc);
        }
    }

    public sealed class ParseException : SchemeException {
        public readonly SourceLocation Loc;

        public ParseException(string message, SourceLocation loc) : base(message, loc) { }
    }
}
