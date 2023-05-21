// SmolScheme/Numerics.cs
//
// Actual numeric representations, as defined in R⁷RS § 6.2.

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
    /// <summary>
    /// Double-precision floating point. These are never exact.
    /// </summary>
    internal sealed class SchemeComplex : SchemeNumber {
        public readonly Complex Value;

        internal SchemeComplex(Complex value) {
            Value = value;
        }

        public override bool Exact {
            get { return false; }
        }

        public override bool IsReal {
            get { return Value.Imaginary == 0.0; }
        }

        public override bool IsInteger {
            get { return IsReal && Math.Floor(Value.Real) == Value.Real; }
        }

        public override double DoubleValue {
            get {
                if (!IsReal)
                    throw new SchemeException("Expected real value but found " + Value);
                return Value.Real;
            }
        }

        public override string StructureToString(ToStringOptions options) {
            return IsReal ? "" + Value.Real : "" + Value;
        }

        public override bool TryBridgeTo(Type type, out object outResult) {
            // Strip off Nullable if necessary.
            Type innerType = Nullable.GetUnderlyingType(type);
            if (innerType != null)
                return TryBridgeTo(innerType, out outResult);

            bool ok;
            switch (Type.GetTypeCode(type)) {
                case TypeCode.Byte:
                case TypeCode.SByte:
                case TypeCode.UInt16:
                case TypeCode.UInt32:
                case TypeCode.UInt64:
                case TypeCode.Int16:
                case TypeCode.Int32:
                case TypeCode.Int64:
                case TypeCode.Decimal:
                    outResult = null;
                    return false;
                case TypeCode.Double:
                    outResult = ((ok = IsReal)) ? (object)Value.Real : null;
                    return ok;
                case TypeCode.Single:
                    outResult = ((ok = IsReal)) ? (object)(float)Value.Real : null;
                    return ok;
            }

            if (type == typeof(Complex)) {
                outResult = Value;
                return true;
            }

            return base.TryBridgeTo(type, out outResult);
        }

        public override bool Eqv(SchemeObject other) {
            return other is SchemeNumber && Value == other.BridgeTo<Complex>();
        }

        public override bool NotEqual(SchemeNumber other) {
            return Value != other.BridgeTo<Complex>();
        }

        public override bool LessThan(SchemeNumber otherObj) {
            Complex other = otherObj.BridgeTo<Complex>();
            if (Value.Real == other.Real)
                return Value.Imaginary < other.Imaginary;
            return Value.Real < other.Real;
        }

        public override bool LessThanEqual(SchemeNumber otherObj) {
            Complex other = otherObj.BridgeTo<Complex>();
            if (Value.Real == other.Real)
                return Value.Imaginary <= other.Imaginary;
            return Value.Real <= other.Real;
        }

        public override bool GreaterThan(SchemeNumber otherObj) {
            Complex other = otherObj.BridgeTo<Complex>();
            if (Value.Real == other.Real)
                return Value.Imaginary > other.Imaginary;
            return Value.Real > other.Real;
        }

        public override bool GreaterThanEqual(SchemeNumber otherObj) {
            Complex other = otherObj.BridgeTo<Complex>();
            if (Value.Real == other.Real)
                return Value.Imaginary >= other.Imaginary;
            return Value.Real >= other.Real;
        }

        public override SchemeNumber Add(SchemeNumber other) {
            return new SchemeComplex(Value + other.BridgeTo<Complex>());
        }

        public override SchemeNumber Subtract(SchemeNumber other) {
            return new SchemeComplex(Value - other.BridgeTo<Complex>());
        }

        public override SchemeNumber Multiply(SchemeNumber other) {
            return new SchemeComplex(Value * other.BridgeTo<Complex>());
        }

        public override SchemeNumber Divide(SchemeNumber other) {
            return new SchemeComplex(Value / other.BridgeTo<Complex>());
        }

        public override SchemeNumber IntegerDivide(
                SchemeNumber other, bool truncate, out SchemeNumber remainder) {
            double n1 = DoubleValue, n2 = other.DoubleValue;
            double nQ = n1 / n2;
            nQ = truncate ? Math.Truncate(nQ) : Math.Floor(nQ);
            double nR = n1 - n2 * nQ;

            remainder = new SchemeComplex((Complex)nR);
            return new SchemeComplex((Complex)nQ);
        }

        public override SchemeNumber Negate() {
            return new SchemeComplex(-Value);
        }

        public override SchemeNumber Min(SchemeNumber other) {
            return new SchemeComplex(
                Complex.MinMagnitude(Value, other.BridgeTo<Complex>()));
        }

        public override SchemeNumber Max(SchemeNumber other) {
            return new SchemeComplex(
                Complex.MaxMagnitude(Value, other.BridgeTo<Complex>()));
        }

        public override SchemeNumber Expt(SchemeNumber other) {
            return new SchemeComplex(Complex.Pow(Value, other.BridgeTo<Complex>()));
        }

        public override SchemeNumber WithExactness(bool exact) {
            return exact ? new SchemeInt((long)Math.Round(Value.Real)) : new SchemeComplex(Value);
        }

        public override SchemeNumber Abs() {
            if (!IsReal)
                throw new SchemeException("`Abs` requires a real number");
            return new SchemeComplex(Math.Abs(Value.Real));
        }

        public override SchemeNumber Floor() {
            if (!IsReal)
                throw new SchemeException("`Floor` requires a real number");
            return new SchemeComplex(Math.Floor(Value.Real));
        }

        public override SchemeNumber Truncate() {
            if (!IsReal)
                throw new SchemeException("`Truncate` requires a real number");
            return new SchemeComplex(Math.Truncate(Value.Real));
        }

        public override SchemeNumber Ceiling() {
            if (!IsReal)
                throw new SchemeException("`Ceiling` requires a real number");
            return new SchemeComplex(Math.Ceiling(Value.Real));
        }

        public override SchemeNumber Round() {
            return new SchemeComplex(Math.Round(Value.Real, MidpointRounding.ToEven));
        }

        /// <summary>
        /// Algorithm is from: https://stackoverflow.com/a/51142807
        /// </summary>
        protected override double ToRational(out double outDenominator) {
            double doubleValue = DoubleValue;

            if (Double.IsInfinity(doubleValue)) {
                outDenominator = 0.0;
                return Math.Sign(doubleValue);
            }

            int bDigits = DBL_MANT_BITS;
            int expo;
            outDenominator = 1.0;
            double numerator = Frexp(doubleValue, out expo) * Math.Pow(2.0, (double)bDigits);
            expo -= bDigits;
            if (expo > 0) {
                numerator *= Math.Pow(2.0, (double)expo);
            } else if (expo < 0) {
                expo = -expo;
                if (expo >= DBL_EXP_MAX - 1) {
                    numerator /= Math.Pow(2.0, (double)(expo - (DBL_EXP_MAX - 1)));
                    outDenominator *= Math.Pow(2.0, (double)(DBL_EXP_MAX - 1));
                    return numerator;
                }
                outDenominator *= Math.Pow(2.0, expo);
            }

            while (numerator != 0.0 && numerator % 2.0 == 0.0 && outDenominator % 2.0 == 0.0) {
                numerator /= 2.0;
                outDenominator /= 2.0;
            }

            return numerator;
        }

        /// <summary>
        /// From https://github.com/MachineCognitis/C.math.NET
        /// </summary>
        private static double Frexp(double number, out int exponent) {
            long bits = System.BitConverter.DoubleToInt64Bits(number);
            int exp = (int)((bits & DBL_EXP_MASK) >> DBL_MANT_BITS);
            exponent = 0;

            if (exp == 0x7ff || number == 0D) {
                number += number;
            } else {
                // Not zero and finite.
                exponent = exp - 1022;
                if (exp == 0) {
                    // Subnormal, scale number so that it is in [1, 2).
                    number *= System.BitConverter.Int64BitsToDouble(0x4350000000000000L); // 2^54
                    bits = System.BitConverter.DoubleToInt64Bits(number);
                    exp = (int)((bits & DBL_EXP_MASK) >> DBL_MANT_BITS);
                    exponent = exp - 1022 - 54;
                }
                // Set exponent to -1 so that number is in [0.5, 1).
                number = System.BitConverter.Int64BitsToDouble(
                    (bits & DBL_EXP_CLR_MASK) | 0x3fe0000000000000L);
            }

            return number;
        }

        private static readonly long DBL_EXP_MASK = 0x7ff0000000000000L;
        private static readonly int DBL_EXP_MAX = 1023;
        private static readonly int DBL_MANT_BITS = 52;
        private static readonly long DBL_SGN_MASK = -1 - 0x7fffffffffffffffL;
        private static readonly long DBL_MANT_MASK = 0x000fffffffffffffL;
        private static readonly long DBL_EXP_CLR_MASK = DBL_SGN_MASK | DBL_MANT_MASK;
    }

    internal sealed class SchemeInt : SchemeNumber {
        public BigInteger Value;

        internal SchemeInt(BigInteger value) {
            Value = value;
        }

        internal SchemeInt(long value) {
            Value = new BigInteger(value);
        }

        internal SchemeInt(ulong value) {
            Value = new BigInteger(value);
        }

        public override bool Exact {
            get { return true; }
        }

        public override bool IsReal {
            get { return true; }
        }

        public override bool IsInteger {
            get { return true; }
        }

        public override double DoubleValue {
            get { return (double)Value; }
        }

        public override bool TryBridgeTo(Type type, out object outResult) {
            // Strip off Nullable if necessary.
            Type innerType = Nullable.GetUnderlyingType(type);
            if (innerType != null)
                return TryBridgeTo(innerType, out outResult);

            checked {
                switch (Type.GetTypeCode(type)) {
                    case TypeCode.Byte:
                        outResult = (object)(byte)Value;
                        return true;
                    case TypeCode.SByte:
                        outResult = (object)(sbyte)Value;
                        return true;
                    case TypeCode.UInt16:
                        outResult = (object)(ushort)Value;
                        return true;
                    case TypeCode.UInt32:
                        outResult = (object)(uint)Value;
                        return true;
                    case TypeCode.Int16:
                        outResult = (object)(short)Value;
                        return true;
                    case TypeCode.Int32:
                        outResult = (object)(int)Value;
                        return true;
                    case TypeCode.Int64:
                        outResult = (object)(long)Value;
                        return true;
                    case TypeCode.UInt64:
                        outResult = (object)(ulong)Value;
                        return true;
                    case TypeCode.Double:
                        outResult = (object)(double)Value;
                        return true;
                    case TypeCode.Single:
                        outResult = (object)(float)Value;
                        return true;
                    case TypeCode.Decimal:
                        outResult = (object)(decimal)Value;
                        return true;
                }
            }

            if (type == typeof(Complex)) {
                outResult = (Complex)(double)Value;
                return true;
            }

            return base.TryBridgeTo(type, out outResult);
        }

        public override string StructureToString(ToStringOptions options) {
            return "" + Value;
        }

        public override bool Eqv(SchemeObject otherObj) {
            if (!(otherObj is SchemeNumber))
                return false;
            var other = (SchemeNumber)otherObj;
            if (other is SchemeInt)
                return Value == ((SchemeInt)other).Value;
            return (double)Value == other.BridgeTo<double>();
        }

        public override bool NotEqual(SchemeNumber other) {
            if (other is SchemeInt)
                return Value != ((SchemeInt)other).Value;
            return (double)Value != other.BridgeTo<double>();
        }

        public override bool LessThan(SchemeNumber other) {
            if (other is SchemeInt)
                return Value < ((SchemeInt)other).Value;
            return (double)Value < other.BridgeTo<double>();
        }

        public override bool LessThanEqual(SchemeNumber other) {
            if (other is SchemeInt)
                return Value <= ((SchemeInt)other).Value;
            return (double)Value <= other.BridgeTo<double>();
        }

        public override bool GreaterThan(SchemeNumber other) {
            if (other is SchemeInt)
                return Value > ((SchemeInt)other).Value;
            return (double)Value > other.BridgeTo<double>();
        }

        public override bool GreaterThanEqual(SchemeNumber other) {
            if (other is SchemeInt)
                return Value >= ((SchemeInt)other).Value;
            return (double)Value >= other.BridgeTo<double>();
        }

        public override SchemeNumber Add(SchemeNumber other) {
            if (other is SchemeInt)
                return new SchemeInt(checked(Value + ((SchemeInt)other).Value));
            Complex z = (Complex)Value;
            return new SchemeComplex(z).Add(other);
        }

        public override SchemeNumber Subtract(SchemeNumber other) {
            if (other is SchemeInt)
                return new SchemeInt(Value - ((SchemeInt)other).Value);
            Complex z = (Complex)Value;
            return new SchemeComplex(z).Subtract(other);
        }

        public override SchemeNumber Multiply(SchemeNumber other) {
            if (other is SchemeInt)
                return new SchemeInt(Value * ((SchemeInt)other).Value);
            Complex z = (Complex)Value;
            return new SchemeComplex(z).Multiply(other);
        }

        public override SchemeNumber Divide(SchemeNumber other) {
            if (other is SchemeInt) {
                BigInteger otherValue = ((SchemeInt)other).Value;
                if (Value % otherValue == 0)
                    return new SchemeInt(Value / otherValue);
            }

            Complex z = (Complex)Value;
            return new SchemeComplex(z).Divide(other);
        }

        public override SchemeNumber Expt(SchemeNumber other) {
            Complex z = (Complex)Value;
            return new SchemeComplex(z).Expt(other);
        }

        public override SchemeNumber IntegerDivide(
                SchemeNumber other, bool truncate, out SchemeNumber remainder) {
            if (other is SchemeInt) {
                remainder = new SchemeInt(Value % ((SchemeInt)other).Value);
                return new SchemeInt(Value / ((SchemeInt)this).Value);
            }

            Complex z = (Complex)Value;
            SchemeComplex lhs = new SchemeComplex(z);
            return lhs.IntegerDivide(other, truncate, out remainder);
        }

        public override SchemeNumber Negate() {
            Complex z = -(Complex)Value;
            return new SchemeComplex(z);
        }

        public override SchemeNumber Min(SchemeNumber other) {
            if (other is SchemeInt)
                return new SchemeInt(BigInteger.Min(Value, ((SchemeInt)other).Value));

            Complex z = Complex.MinMagnitude((Complex)Value, other.BridgeTo<Complex>());
            return new SchemeComplex(z);
        }

        public override SchemeNumber Max(SchemeNumber other) {
            if (other is SchemeInt)
                return new SchemeInt(BigInteger.Max(Value, ((SchemeInt)other).Value));

            Complex z = Complex.MaxMagnitude((Complex)Value, other.BridgeTo<Complex>());
            return new SchemeComplex(z);
        }

        public override SchemeNumber WithExactness(bool exact) {
            return exact ? new SchemeInt(Value) : new SchemeComplex((Complex)Value);
        }

        public override SchemeNumber Abs() {
            return new SchemeInt(BigInteger.Abs(Value));
        }

        public override SchemeNumber Floor() {
            return this;
        }

        public override SchemeNumber Truncate() {
            return this;
        }

        public override SchemeNumber Ceiling() {
            return this;
        }

        public override SchemeNumber Round() {
            return this;
        }

        protected override double ToRational(out double outDenominator) {
            outDenominator = 1.0;
            return (double)Value;
        }

        /// <summary>
        /// From https://stackoverflow.com/a/14041868
        /// </summary>
        internal static BigInteger ParseBigInt(string digits, int radix) {
            if (digits.Length == 0)
                throw new BigIntParseException("String can't be empty");

            int sign;
            if (digits[0] == '-') {
                sign = -1;
                digits = digits.Substring(1);
            } else {
                sign = 1;
            }

            BigInteger result = digits.Aggregate(new BigInteger(), (number, digit) => {
                int value;
                if (digit >= '0' && digit <= '9') {
                    value = (int)(digit - '0');
                } else if (digit >= 'A' && digit <= 'F') {
                    value = (int)(digit - 'A' + 10);
                } else if (digit >= 'a' && digit <= 'f') {
                    value = (int)(digit - 'a' + 10);
                } else {
                    throw new BigIntParseException(String.Format("Expected digit but found `{0}`",
                        digit));
                }

                if (value >= radix) {
                    throw new BigIntParseException(String.Format(
                        "Invalid digit for number of radix {0}: `{1}`", radix, digit));
                }

                return number * radix + value;
            });

            return result * sign;
        }
    }

    internal sealed class BigIntParseException : Exception {
        public BigIntParseException(string message) : base(message) { }
    }
}
