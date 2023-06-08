// SmolScheme/Proc.cs

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
    [SchemeBridge.TypeTest("Procedure?")]
    public abstract class SchemeProc : SchemeObject {
        public SchemeProc() : base() { }

        internal sealed override SExpr ReplaceIdentifiers(Dictionary<string, SExpr> replacements) {
            return this;
        }

        internal sealed override void RecursivelyReplaceDatumLabel(int label, SExpr replacement) { }

        public override bool TryBridgeTo(Type type, out object outResult) {
            if (typeof(Delegate).IsAssignableFrom(type) &&
                    ProcBridge.BridgeProc(this, type, out outResult)) {
                return true;
            }

            return base.TryBridgeTo(type, out outResult);
        }
    }

    internal class SchemeProcDatum : SchemeProc {
        public readonly CaseLambdaClause[] Clauses;
        public readonly Env Env;

        public SchemeProcDatum(CaseLambdaClause[] clauses, Env env) : base() {
            Clauses = clauses;
            Env = env;
        }

        public SchemeProcDatum(CaseLambdaClause clause, Env env) :
            this(new CaseLambdaClause[] { clause }, env) { }

        public override string StructureToString(ToStringOptions options) {
            if (!options.ContainsFlags(ToStringFlags.MJSExprForm)) {
                if (Clauses.Length == 1)
                    return String.Format("(lambda {0})", Clauses[0].ToString(options));
                return String.Format(
                    "(case-lambda {0})",
                    String.Join(
                        " ",
                        Clauses.Select(clause => "(" + clause.ToString(options) + ")").ToArray()));
            }

            if (Clauses.Length == 1)
                return "Lambda[" + Clauses[0].ToString(options) + "]";
            return "CaseLambda[" +
                String.Join(
                    ", ",
                    Clauses.Select(clause => "{" + clause.ToString(options) + "}").ToArray()) + "]";
        }

        public override bool Eqv(SchemeObject other) {
            return this == other;
        }
    }

    public abstract class NativeProc : SchemeProc {
        public NativeProc() : base() { }

        public abstract Evaluation Call(
            SchemeObject[] args, Env env, DynamicEnv dynEnv, Mode mode);

        public override string StructureToString(ToStringOptions options) {
            return "" + GetType();
        }

        public static bool RequireBool(SchemeObject value) {
            if (value is SchemeBool)
                return ((SchemeBool)value).Value;
            throw new SchemeException("Expected boolean but found " + value.GetType());
        }

        public static IEnumerable<SchemeObject> RequireList(SchemeObject value) {
            if (value is SchemeList)
                return (SchemeList)value;
            throw new SchemeException("Expected list but found " + value.GetType());
        }

        public static string RequireSymbol(SchemeObject value) {
            if (value is Symbol)
                return ((Symbol)value).Name;
            throw new SchemeException("Expected symbol but found " + value.GetType());
        }

        public static T RequireForeignObject<T>(SchemeObject value) {
            if (value is NativeObject) {
                var foreignValue = (NativeObject)value;
                if (foreignValue.Value == null || foreignValue.Value is T)
                    return (T)foreignValue.Value;
            }
            throw new SchemeException("Expected " + typeof(T) + " but found " + value.GetType());
        }

        protected static void CheckArity(SchemeObject[] args, int minCount, int maxCount) {
            if (!SExprUtils.CheckArity(args.Length, minCount, maxCount))
                SExprUtils.ReportArityError("procedure", args.Length, minCount, maxCount);
        }

        protected static void CheckArity(SchemeObject[] args, int count) {
            CheckArity(args, count, count);
        }

        public static SchemeObject GetKeywordArg(IEnumerable<SchemeObject> args, string keyword) {
            foreach (SchemeObject arg in args) {
                if (!(arg is SchemeList))
                    continue;
                var pair = (SchemeList)arg;
                if (!(pair.Head is Symbol))
                    continue;
                var symbol = (Symbol)pair.Head;
                if (symbol.Name == keyword)
                    return pair.Tail;
            }

            return null;
        }

        public static SchemeObject RequireKeywordArg(IEnumerable<SchemeObject> args, string keyword) {
            SchemeObject value = GetKeywordArg(args, keyword);
            if (value == null)
                throw new SchemeException("Required keyword argument \"" + keyword + "\" not found");
            return value;
        }

        public override bool Eqv(SchemeObject other) {
            return this == other;
        }
    }

    /// <summary>
    /// A convenience class that provides a nicer interface than the one taking the environments
    /// explicitly.
    /// </summary>
    public abstract class SimpleNativeProc : NativeProc {
        private static ThreadLocal<DynamicEnv> sCurrentDynamicEnv = new ThreadLocal<DynamicEnv>();

        public static DynamicEnv CurrentDynamicEnv {
            get { return sCurrentDynamicEnv.Value; }
            internal set { sCurrentDynamicEnv.Value = value; }
        }

        protected abstract SchemeObject Call(SchemeObject[] args);

        public sealed override Evaluation Call(
                SchemeObject[] args, Env env, DynamicEnv dynEnv, Mode mode) {
            DynamicEnv oldDynEnv = sCurrentDynamicEnv.Value;
            sCurrentDynamicEnv.Value = dynEnv;

            try {
                return new Result(Call(args));
            } finally {
                sCurrentDynamicEnv.Value = oldDynEnv;
            }
        }
    }

    /// <summary>
    /// A native procedure that does a type test for a known type at runtime.
    /// </summary>
    internal class IsTypeProc : TestProc {
        Type mType;

        public IsTypeProc(Type type) {
            mType = type;
        }

        protected override bool Check(SchemeObject arg) {
            return mType.IsAssignableFrom(arg.GetType());
        }
    }

    /// <summary>
    /// A native procedure that does a type test for a known type wrapped in a NativeObject wrapper
    /// at runtime.
    /// </summary>
    internal class IsNativeTypeProc : TestProc {
        Type mType;

        public IsNativeTypeProc(Type type) {
            mType = type;
        }

        protected override bool Check(SchemeObject arg) {
            return arg is NativeObject &&
                mType.IsAssignableFrom(((NativeObject)arg).Value.GetType());
        }
    }

}
