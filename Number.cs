// SmolScheme/Number.cs
//
// Abstract interface for numbers, as defined in R⁷RS § 6.2.

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
    [SchemeBridge.TypeTest("Number?")]
    public abstract class SchemeNumber : SimpleDatum {
        public abstract bool Exact { get; }

        public abstract bool IsReal { get; }
        public abstract bool IsInteger { get; }

        public abstract double DoubleValue { get; }


        [SchemeBridge("Complex?")]
        public static bool ObjectIsComplex(SchemeObject arg) {
            return arg is SchemeNumber;
        }

        [SchemeBridge("Real?")]
        public static bool ObjectIsReal(SchemeObject arg) {
            return arg is SchemeNumber && ((SchemeNumber)arg).IsReal;
        }

        [SchemeBridge("Rational?")]
        public static bool ObjectIsRational(SchemeObject arg) {
            return arg is SchemeNumber && ((SchemeNumber)arg).IsReal;
        }

        [SchemeBridge("Integer?")]
        public static bool ObjectIsInteger(SchemeObject arg) {
            return arg is SchemeNumber && ((SchemeNumber)arg).IsInteger;
        }

        [SchemeBridge("Exact?")]
        public static bool ObjectIsExact(SchemeObject arg) {
            return arg is SchemeNumber && ((SchemeNumber)arg).Exact;
        }

        [SchemeBridge("Inexact?")]
        public static bool ObjectIsInexact(SchemeObject arg) {
            return arg is SchemeNumber && !((SchemeNumber)arg).Exact;
        }

        private static readonly SchemeNumber Zero = SchemeNumber.Create(0);

        private static readonly SchemeNumber One = SchemeNumber.Create(1);

        public bool IsZero {
            [SchemeBridge("Zero?")]
            get { return Eqv(Zero); }
        }

        public bool IsPositive {
            [SchemeBridge("Positive?")]
            get { return GreaterThan(Zero); }
        }

        public bool IsNegative {
            [SchemeBridge("Negative?")]
            get { return LessThan(Zero); }
        }

        public bool IsEven {
            [SchemeBridge("Even?")]
            get { return Modulo(SchemeNumber.Create(2)).Eqv(Zero); }
        }

        public bool IsOdd {
            [SchemeBridge("Odd?")]
            get { return !IsEven; }
        }

        public double Numerator {
            [SchemeBridge("Numerator")]
            get {
                double denom;
                return ToRational(out denom);
            }
        }

        public double Denominator {
            [SchemeBridge("Denominator")]
            get {
                double denom;
                ToRational(out denom);
                return denom;
            }
        }

        public static SchemeNumber Create(byte value, bool exact = true) {
            return exact ? new SchemeInt((ulong)value) : new SchemeComplex((Complex)value);
        }

        public static SchemeNumber Create(sbyte value, bool exact = true) {
            return exact ? new SchemeInt((long)value) : new SchemeComplex((Complex)value);
        }

        public static SchemeNumber Create(short value, bool exact = true) {
            return exact ? new SchemeInt((long)value) : new SchemeComplex((Complex)value);
        }

        public static SchemeNumber Create(ushort value, bool exact = true) {
            return exact ? new SchemeInt((ulong)value) : new SchemeComplex((Complex)value);
        }

        public static SchemeNumber Create(int value, bool exact = true) {
            return exact ? new SchemeInt((int)value) : new SchemeComplex((Complex)value);
        }

        public static SchemeNumber Create(uint value, bool exact = true) {
            return exact ? new SchemeInt((uint)value) : new SchemeComplex((Complex)value);
        }

        public static SchemeNumber Create(long value, bool exact = true) {
            return exact ? new SchemeInt(value) : new SchemeComplex((Complex)value);
        }

        public static SchemeNumber Create(ulong value, bool exact = true) {
            return exact ? new SchemeInt(value) : new SchemeComplex((Complex)value);
        }

        public static SchemeNumber Create(BigInteger value, bool exact = true) {
            return exact ? new SchemeInt(value) : new SchemeComplex((Complex)value);
        }

        public static SchemeNumber Create(float value, bool exact = false) {
            return new SchemeComplex((Complex)value);
        }

        public static SchemeNumber Create(double value, bool exact = false) {
            return new SchemeComplex((Complex)value);
        }

        public static SchemeNumber Create(decimal value, bool exact = false) {
            return new SchemeComplex((Complex)value);
        }

        public static SchemeNumber Create(Complex value, bool exact = false) {
            return new SchemeComplex(value);
        }

        internal static SchemeNumber Create(NumberToken token) {
            if (token is IntToken)
                return Create(((IntToken)token).Value, token.Exact);
            if (token is DoubleToken)
                return Create(((DoubleToken)token).Value, token.Exact);
            throw new Exception("Unknown numeric token type");
        }

        public bool IsExactInteger {
            [SchemeBridge("ExactInteger?")]
            get { return Exact && IsInteger; }
        }

        internal sealed override SExpr Unquote() {
            return this;
        }

        [SchemeBridge("Inexact")]
        public SchemeNumber ToInexact() {
            return WithExactness(false);
        }

        [SchemeBridge("Exact")]
        public SchemeNumber ToExact() {
            return WithExactness(true);
        }

        [SchemeBridge("Floor/")]
        public SchemeList FloorDivide(SchemeNumber other) {
            SchemeNumber remainder;
            SchemeNumber quotient = IntegerDivide(other, false, out remainder);
            return new SchemeList(quotient, new SchemeObject[] { remainder });
        }

        [SchemeBridge("Truncate/")]
        public SchemeList TruncateDivide(SchemeNumber other) {
            SchemeNumber remainder;
            SchemeNumber quotient = IntegerDivide(other, true, out remainder);
            return new SchemeList(quotient, new SchemeObject[] { remainder });
        }

        [SchemeBridge]
        public SchemeNumber FloorQuotient(SchemeNumber other) {
            return (SchemeNumber)FloorDivide(other)[0];
        }

        [SchemeBridge]
        public SchemeNumber FloorRemainder(SchemeNumber other) {
            return (SchemeNumber)FloorDivide(other)[1];
        }

        [SchemeBridge]
        public SchemeNumber TruncateQuotient(SchemeNumber other) {
            return (SchemeNumber)TruncateDivide(other)[0];
        }

        [SchemeBridge]
        public SchemeNumber TruncateRemainder(SchemeNumber other) {
            return (SchemeNumber)TruncateDivide(other)[1];
        }

        [SchemeBridge]
        public SchemeNumber Quotient(SchemeNumber other) {
            return TruncateQuotient(other);
        }

        [SchemeBridge]
        public SchemeNumber Remainder(SchemeNumber other) {
            return TruncateRemainder(other);
        }

        [SchemeBridge]
        public SchemeNumber Modulo(SchemeNumber other) {
            return FloorRemainder(other);
        }

        /// <summary>
        /// Computes the greatest common denominator of the integers per R⁷RS § 6.2.6.
        ///
        /// This uses the Euclidean algorithm:
        /// https://en.wikipedia.org/wiki/Euclidean_algorithm#Implementations
        /// </summary>
        [SchemeBridge]
        public static SchemeNumber Gcd(params SchemeNumber[] values) {
            if (values.Length == 0)
                return Zero;

            SchemeNumber first = values[0].Abs();
            List<SchemeNumber> rest = values.Skip(1).ToList();
            while (rest.Count > 0) {
                SchemeNumber a = first;
                SchemeNumber b = rest[rest.Count - 1].Abs();
                rest.RemoveAt(rest.Count - 1);

                while (!b.IsZero) {
                    SchemeNumber t = b;
                    b = a.Remainder(b);
                    a = t;
                }

                first = a;
            }

            return first;
        }

        /// <summary>
        /// Computes the least common multiple of the integers per R⁷RS § 6.2.6.
        ///
        /// This uses the GCD-based algorithm:
        /// https://en.wikipedia.org/wiki/Least_common_multiple#Using_the_greatest_common_divisor
        /// </summary>
        [SchemeBridge]
        public static SchemeNumber Lcm(params SchemeNumber[] values) {
            if (values.Length == 0)
                return SchemeNumber.Create(1);

            SchemeNumber first = values[0];
            List<SchemeNumber> rest = values.Skip(1).ToList();
            while (rest.Count > 0) {
                SchemeNumber a = first;
                SchemeNumber b = rest[rest.Count - 1];
                rest.RemoveAt(rest.Count - 1);

                first = (a * b).Abs().Quotient(Gcd(a, b));
            }

            return first;
        }

        /// <summary>
        /// Converts a number to a rational number per R⁷RS § 6.2.6.
        ///
        /// This algorithm is from Chibi Scheme. The comment there says "adapted from Bawden's
        /// algorithm".
        /// </summary>
        [SchemeBridge]
        public SchemeNumber Rationalize(SchemeNumber e) {
            var finish = (SchemeNumber num, SchemeNumber den) =>
                this.IsNegative ? -num / den : num / den;
            SchemeNumber xMag = Abs(), eMag = e.Abs();
            return Rationalize(xMag - eMag, xMag + eMag, finish);
        }

        private static SchemeNumber Rationalize(
                SchemeNumber x,
                SchemeNumber y,
                Func<SchemeNumber, SchemeNumber, SchemeNumber> finish) {
            SchemeNumber fx = x.Floor(), fy = y.Floor();
            if (fx.GreaterThanEqual(x))
                return finish(fx, One);
            if (fx.Eqv(fy))
                return Rationalize(One / (y - fy), One / (x - fx), (n, d) => finish(d + fx * n, n));
            return finish(fx + One, One);
        }

        [SchemeBridge]
        public SchemeNumber Square() {
            return this * this;
        }

        /// <summary>
        /// Computes the exact integer square root per R⁷RS § 6.2.6.
        ///
        /// The algorithm uses binary search:
        /// https://en.wikipedia.org/wiki/Integer_square_root#Algorithm_using_binary_search
        /// </summary>
        [SchemeBridge]
        public SchemeNumber ExactIntegerSqrt(out SchemeNumber outRemainder) {
            SchemeNumber two = SchemeNumber.Create(2);

            SchemeNumber l = Zero, r = this + One;
            while (l.LessThan(r - One)) {
                SchemeNumber m = (l + r).Quotient(two);
                if (m.Square().LessThanEqual(this))
                    l = m;
                else
                    r = m;
            }
            outRemainder = this - l.Square();
            return l;
        }

        public static SchemeNumber operator +(SchemeNumber lhs, SchemeNumber rhs) {
            return lhs.Add(rhs);
        }

        public static SchemeNumber operator -(SchemeNumber lhs, SchemeNumber rhs) {
            return lhs.Subtract(rhs);
        }

        public static SchemeNumber operator *(SchemeNumber lhs, SchemeNumber rhs) {
            return lhs.Multiply(rhs);
        }

        public static SchemeNumber operator /(SchemeNumber lhs, SchemeNumber rhs) {
            return lhs.Divide(rhs);
        }

        public static SchemeNumber operator -(SchemeNumber n) {
            return n.Negate();
        }

        [SchemeBridge("+")]
        public static SchemeNumber AddAll(params SchemeNumber[] values) {
            if (values.Length == 0)
                return Zero;

            SchemeNumber result = values[0];
            for (int i = 1; i < values.Length; i++)
                result += values[i];

            return result;
        }

        [SchemeBridge("-")]
        public static SchemeNumber SubtractAll(SchemeNumber first, params SchemeNumber[] rest) {
            if (rest.Length == 0)
                return first.Negate();

            SchemeNumber result = first;
            foreach (SchemeNumber arg in rest)
                result -= arg;

            return result;
        }

        [SchemeBridge("*")]
        public static SchemeNumber MultiplyAll(params SchemeNumber[] values) {
            if (values.Length == 0)
                return One;

            SchemeNumber result = values[0];
            for (int i = 1; i < values.Length; i++)
                result *= values[i];

            return result;
        }

        [SchemeBridge("/")]
        public static SchemeNumber DivideAll(SchemeNumber first, params SchemeNumber[] rest) {
            if (rest.Length == 0)
                return One / first;

            SchemeNumber result = first;
            foreach (SchemeNumber arg in rest)
                result /= arg;

            return result;
        }

        [SchemeBridge("Min")]
        public static SchemeNumber MinAll(SchemeNumber first, params SchemeNumber[] rest) {
            SchemeNumber result = first;
            foreach (SchemeNumber arg in rest)
                result = result.Min(arg);
            return result;
        }

        [SchemeBridge("Max")]
        public static SchemeNumber MaxAll(SchemeNumber first, params SchemeNumber[] rest) {
            SchemeNumber result = first;
            foreach (SchemeNumber arg in rest)
                result = result.Max(arg);
            return result;
        }

        public abstract bool NotEqual(SchemeNumber other);
        public abstract bool LessThan(SchemeNumber other);
        public abstract bool LessThanEqual(SchemeNumber other);
        public abstract bool GreaterThan(SchemeNumber other);
        public abstract bool GreaterThanEqual(SchemeNumber other);

        public abstract SchemeNumber Add(SchemeNumber other);
        public abstract SchemeNumber Subtract(SchemeNumber other);
        public abstract SchemeNumber Multiply(SchemeNumber other);
        public abstract SchemeNumber Divide(SchemeNumber other);

        public abstract SchemeNumber IntegerDivide(
            SchemeNumber other, bool truncate, out SchemeNumber remainder);

        public abstract SchemeNumber Negate();
        public abstract SchemeNumber Min(SchemeNumber other);
        public abstract SchemeNumber Max(SchemeNumber other);

        [SchemeBridge]
        public abstract SchemeNumber Expt(SchemeNumber other);

        public abstract SchemeNumber WithExactness(bool exact);

        [SchemeBridge("Abs")]
        public abstract SchemeNumber Abs();
        [SchemeBridge("Floor")]
        public abstract SchemeNumber Floor();
        [SchemeBridge("Truncate")]
        public abstract SchemeNumber Truncate();
        [SchemeBridge("Ceiling")]
        public abstract SchemeNumber Ceiling();
        [SchemeBridge("Round")]
        public abstract SchemeNumber Round();

        protected abstract double ToRational(out double outDenominator);
    }

}