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
            try {
                if (typeof(Delegate).IsAssignableFrom(type)) {
                    MethodInfo methodInfo = type.GetMethod("Invoke");
                    ParameterInfo[] parameters = methodInfo.GetParameters();

                    Type[] genericTypes = new Type[1 + parameters.Length];
                    genericTypes[0] = methodInfo.ReturnType;
                    for (int i = 0; i < parameters.Length; i++)
                        genericTypes[i + 1] = parameters[i].ParameterType;

                    ProcBridge procBridge;
                    if (methodInfo.ReturnType == typeof(void)) {
                        switch (parameters.Length) {
                            case 0:
                                procBridge = (ProcBridge)Activator.CreateInstance(
                                    typeof(ProcVoidBridge0),
                                    type,
                                    this,
                                    Interpreter.Current);
                                break;
                            default:
                                // TODO: More arities.
                                return base.TryBridgeTo(type, out outResult);
                        }
                    } else {
                        switch (parameters.Length) {
                            case 0:
                                procBridge = (ProcBridge)Activator.CreateInstance(
                                    typeof(ProcBridge0<>).MakeGenericType(genericTypes),
                                    type,
                                    this,
                                    Interpreter.Current);
                                break;
                            case 1:
                                procBridge = (ProcBridge)Activator.CreateInstance(
                                    typeof(ProcBridge1<,>).MakeGenericType(genericTypes),
                                    type,
                                    this,
                                    Interpreter.Current);
                                break;
                            case 2:
                                procBridge = (ProcBridge)Activator.CreateInstance(
                                    typeof(ProcBridge2<,,>).MakeGenericType(genericTypes),
                                    type,
                                    this,
                                    Interpreter.Current);
                                break;
                            default:
                                // TODO: More arities.
                                return base.TryBridgeTo(type, out outResult);
                        }
                    }

                    outResult = Delegate.CreateDelegate(type, procBridge, "Call");
                    return true;
                }
            } catch (Exception e) {
                Console.WriteLine("Exception! " + e);
                throw e;
            }

            return base.TryBridgeTo(type, out outResult);
        }

        private abstract class ProcBridge {
            Type mType;
            SchemeProc mProc;

            // It is possible that the proc is called on a different thread we've never seen
            // before. This happens a lot in async code. To guard against this possibility we need
            // to actually store the interpreter here as opposed to using `Interpreter.Current`
            // when the proc is called.
            Interpreter mInterpreter;

            public ProcBridge(Type type, SchemeProc proc, Interpreter interp) {
                mType = type;
                mProc = proc;
                mInterpreter = interp;
            }

            protected object DoCall(object[] suppliedArgs) {
                try {
                    MethodInfo method = mType.GetMethod("Invoke");
                    ParameterInfo[] expectedArgs = method.GetParameters();
                    bool hasParamsArg = expectedArgs.Length > 0 && expectedArgs[
                        expectedArgs.Length - 1].GetCustomAttribute<ParamArrayAttribute>() != null;

                    if ((!hasParamsArg && suppliedArgs.Length != expectedArgs.Length) ||
                        (hasParamsArg && suppliedArgs.Length < expectedArgs.Length)) {
                        throw new SchemeException(
                            String.Format(
                                "This procedure takes {0} {1} but {2} {3} supplied",
                                hasParamsArg ? "at least" : "exactly",
                                SExprUtils.Pluralize("argument", expectedArgs.Length),
                                SExprUtils.Pluralize("argument", suppliedArgs.Length),
                                suppliedArgs.Length == 1 ? "was" : "were"));
                    }

                    Array suppliedParamsArg;
                    int normalArgCount, bridgedArgsCount;
                    if (hasParamsArg) {
                        suppliedParamsArg = (Array)suppliedArgs[suppliedArgs.Length - 1];
                        normalArgCount = suppliedArgs.Length - 1;
                        bridgedArgsCount = normalArgCount + suppliedParamsArg.Length;
                    } else {
                        suppliedParamsArg = null;
                        bridgedArgsCount = normalArgCount = suppliedArgs.Length;
                    }

                    SExpr[] bridgedArgs = new SExpr[bridgedArgsCount];

                    for (int i = 0; i < normalArgCount; i++)
                        bridgedArgs[i] = Bridge(suppliedArgs[i]);

                    if (hasParamsArg) {
                        for (int i = 0; i < suppliedParamsArg.Length; i++) {
                            bridgedArgs[normalArgCount + i] =
                                Bridge(suppliedParamsArg.GetValue(i));
                        }
                    }

                    ListExpr callExpr = new ListExpr(mProc, bridgedArgs);
                    SchemeObject returnValue = mInterpreter.Evaluate(
                        callExpr, new Env(), SimpleNativeProc.CurrentDynamicEnv);

                    return returnValue.BridgeTo(method.ReturnType);
                } catch (Exception e) {
                    Console.WriteLine("Exception! " + e);
                    throw e;
                }
            }
        }

        private class ProcVoidBridge0 : ProcBridge {
            public ProcVoidBridge0(Type type, SchemeProc proc, Interpreter interp) :
                base(type, proc, interp) {}

            protected void Call() {
                DoCall(new object[0]);
            }
        }

        private class ProcBridge0<R> : ProcBridge {
            public ProcBridge0(Type type, SchemeProc proc, Interpreter interp) :
                base(type, proc, interp) {}

            protected R Call() {
                return (R)DoCall(new object[0]);
            }
        }

        private class ProcBridge1<R, A> : ProcBridge {
            public ProcBridge1(Type type, SchemeProc proc, Interpreter interp) :
                base(type, proc, interp) {}

            protected R Call(A arg0) {
                return (R)DoCall(new object[] { arg0 });
            }
        }

        private class ProcBridge2<R, A, B> : ProcBridge {
            public ProcBridge2(Type type, SchemeProc proc, Interpreter interp) :
                base(type, proc, interp) {}

            protected R Call(A arg0, B arg1) {
                return (R)DoCall(new object[] { arg0, arg1 });
            }
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
            SchemeObject[] args, Env env, DynamicEnv dynEnv, bool allowDefine);

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
                SchemeObject[] args, Env env, DynamicEnv dynEnv, bool allowDefine) {
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
    /// A native procedure that uses reflection to bridge to a .NET method.
    /// </summary>
    public class BridgingNativeProc : NativeProc {
        private MethodBase[] mMethods;
        private object mTarget;

        /// <summary>
        /// This type must be a delegate.
        /// </summary>
        public BridgingNativeProc(Type delegateType, object target) {
            Debug.Assert(typeof(Delegate).IsAssignableFrom(delegateType));
            mMethods = new MethodBase[] { delegateType.GetMethod("Invoke") };
            mTarget = target;
        }

        public BridgingNativeProc(Delegate del) : this(del.GetType(), del.Target) {}

        public BridgingNativeProc(MethodBase method) : this(new MethodBase[] { method }) { }

        public BridgingNativeProc(MethodBase[] methods) {
            mMethods = methods;
            mTarget = mMethods[0].DeclaringType;
        }

        private BridgeCallResult TryCall(MethodBase method, SchemeObject[] suppliedArgs) {
            // Bridge receiver.
            int suppliedArgIndex = 0;
            object bridgedReceiver;
            if (method.IsStatic || method is ConstructorInfo) {
                bridgedReceiver = null;
            } else {
                if (suppliedArgs.Length == 0)
                    return new BridgeCallArityMismatch(method);
                SchemeObject receiver = suppliedArgs[suppliedArgIndex++];
                if (!receiver.TryBridgeTo(method.DeclaringType, out bridgedReceiver))
                    return new BridgeCallReceiverMismatch(method);
            }

            // This is initialized lazily, in case there are no keyword arguments.
            Dictionary<string, int> parameterNameToIndex = null;
            ParameterInfo[] parameters = method.GetParameters();
            object[] bridgedArgs = new object[parameters.Length];

            // First, scan for params and out args.
            List<object> paramsArgs;
            List<int> outArgIndices;
            ulong targetArgBitmask = FindParamsAndOutArguments(
                parameters, out paramsArgs, out outArgIndices);

            BridgeCallFailure failure = null;

            // Find the first positional argument.
            int nextTargetArgIndex = 0;
            while ((targetArgBitmask & (1UL << nextTargetArgIndex)) != 0)
                nextTargetArgIndex++;

            // Loop through each supplied argument and bridge it.
            while (suppliedArgIndex < suppliedArgs.Length) {
                SchemeObject arg = suppliedArgs[suppliedArgIndex];

                // Which argument are we filling out?
                int targetArgIndex = GetTargetArgIndex(
                    ref arg,
                    nextTargetArgIndex,
                    method,
                    suppliedArgs,
                    ref suppliedArgIndex,
                    parameters,
                    ref parameterNameToIndex,
                    out failure);
                if (targetArgIndex < 0)
                    return failure;

                // Try to bridge that argument.
                failure = TryBridgeArg(
                    parameters,
                    bridgedArgs,
                    ref paramsArgs,
                    ref targetArgBitmask,
                    method,
                    arg,
                    targetArgIndex,
                    suppliedArgIndex);
                if (failure != null)
                    return failure;

                // Advance to the next positional argument.
                while ((targetArgBitmask & (1UL << nextTargetArgIndex)) != 0)
                    nextTargetArgIndex++;

                suppliedArgIndex++;
            }

            // We're done processing arguments. Finish up.
            failure = FinalizeArgumentBridging(
                method, parameters, bridgedArgs, paramsArgs, targetArgBitmask);
            if (failure != null)
                return failure;

            return CallWithBridgedArgs(
                method, bridgedReceiver, bridgedArgs.ToArray(), outArgIndices);
        }

        /// <summary>
        /// Gathers up all params and out arguments and returns an initial target argument bitmask.
        /// </summary>
        private ulong FindParamsAndOutArguments(
                ParameterInfo[] parameters,
                out List<object> outParamsArgs,
                out List<int> outOutArgIndices) {
            // TODO: We may want this to be a bignum...
            ulong targetArgBitmask = 0;

            outParamsArgs = null;
            outOutArgIndices = null;

            for (int i = 0; i < parameters.Length; i++) {
                if (parameters[i].GetCustomAttribute<ParamArrayAttribute>() != null) {
                    outParamsArgs = new List<object>();
                    targetArgBitmask |= 1UL << i;
                }
                if (parameters[i].IsOut) {
                    if (outOutArgIndices == null)
                        outOutArgIndices = new List<int>();
                    outOutArgIndices.Add(i);
                    targetArgBitmask |= 1UL << i;
                }
            }

            return targetArgBitmask;
        }

        /// <summary>
        /// Invokes a method and packages up the result, given a set of bridged arguments.
        /// </summary>
        private BridgeCallResult CallWithBridgedArgs(
                MethodBase method,
                object bridgedReceiver,
                object[] bridgedArgs,
                List<int> outArgIndices) {
            // OK, this call typechecked. Go ahead and invoke the method.
            object result;
            if (method is MethodInfo)
                result = method.Invoke(bridgedReceiver, bridgedArgs);
            else
                result = ((ConstructorInfo)method).Invoke(bridgedArgs);

            // Extract the return value.
            int outArgCount = outArgIndices == null ? 0 : outArgIndices.Count;
            SchemeObject[] returnValues = new SchemeObject[1 + outArgCount];
            if (method is MethodInfo && ((MethodInfo)method).ReturnType == typeof(void))
                returnValues[0] = SchemeNull.Null;
            else
                returnValues[0] = SchemeObject.Bridge(result);

            // Package the return value up with any out parameters, and return.
            for (int i = 0; i < outArgCount; i++)
                returnValues[i + 1] = SchemeObject.Bridge(bridgedArgs[outArgIndices[i]]);
            return new BridgeCallSuccess(returnValues);
        }

        /// <summary>
        /// Returns the index of the argument we need to bridge to, taking keyword arguments into
        /// account.
        /// </summary>
        private int GetTargetArgIndex(
                ref SchemeObject arg,
                int nextTargetArgIndex,
                MethodBase method,
                SchemeObject[] suppliedArgs,
                ref int suppliedArgIndex,
                ParameterInfo[] parameters,
                ref Dictionary<string, int> parameterNameToIndex,
                out BridgeCallFailure outFailure) {
            if (!(arg is Srfi88.Keyword)) {
                outFailure = null;
                return nextTargetArgIndex;
            }

            // Keyword argument.

            // Initialize this table if it hasn't been initialized yet.
            if (parameterNameToIndex == null) {
                parameterNameToIndex = new Dictionary<string, int>();
                for (int i = 0; i < parameters.Length; i++) {
                    // We can't refer to params arguments by keyword.
                    ParameterInfo parameterInfo = parameters[i];
                    if (parameterInfo.GetCustomAttribute<ParamArrayAttribute>() == null)
                        parameterNameToIndex[parameterInfo.Name] = i;
                }
            }

            var keyword = (Srfi88.Keyword)arg;
            if (!parameterNameToIndex.ContainsKey(keyword.Name)) {
                outFailure = new BridgeCallKeywordMismatch(keyword.Name, method);
                return -1;
            }

            outFailure = null;
            arg = suppliedArgs[++suppliedArgIndex];
            return parameterNameToIndex[keyword.Name];
        }

        private BridgeCallFailure TryBridgeArg(
                ParameterInfo[] parameters,
                object[] bridgedArgs,
                ref List<object> paramsArgs,
                ref ulong targetArgBitmask,
                MethodBase method,
                SchemeObject arg,
                int targetArgIndex,
                int suppliedArgIndex) {
            // Is the argument index past the list of valid argument indices?
            if (targetArgIndex >= parameters.Length) {
                if (parameters.Length == 0)
                    return new BridgeCallArityMismatch(method);

                // This might be OK if we have a params arg and we can just add to that. But if
                // not, we're out of luck.
                if (paramsArgs == null)
                    return new BridgeCallArityMismatch(method);

                // Bridge to an element of the params type.
                Type elementType = parameters.Last().ParameterType.GetElementType();
                object bridgedParamArg = null;
                if (!arg.TryBridgeTo(elementType, out bridgedParamArg))
                    return new BridgeCallTypeMismatch(targetArgIndex, suppliedArgIndex, method);

                paramsArgs.Add(bridgedParamArg);
                return null;
            }

            // Is this the first argument of a params attribute? 
            ParameterInfo parameterInfo = parameters[targetArgIndex];
            if (parameterInfo.GetCustomAttribute<ParamArrayAttribute>() != null) {
                Debug.Assert(targetArgIndex == parameters.Length - 1);

                // Bridge to the type.
                Type elementType = parameters.Last().ParameterType.GetElementType();
                object bridgedParamArg = null;
                if (!arg.TryBridgeTo(elementType, out bridgedParamArg))
                    return new BridgeCallTypeMismatch(targetArgIndex, suppliedArgIndex, method);

                paramsArgs.Add(bridgedParamArg);
                return null;
            }

            // Has the programmer already supplied this argument? 
            if ((targetArgBitmask & (1UL << targetArgIndex)) != 0UL) {
                // Yes. This is an error.
                return new BridgeDuplicateArg(parameters[targetArgIndex].Name, method);
            }

            // This is a regular old argument. Bridge to the right type.
            object bridgedArg = null;
            if (!arg.TryBridgeTo(parameterInfo.ParameterType, out bridgedArg))
                return new BridgeCallTypeMismatch(targetArgIndex, suppliedArgIndex, method);
            bridgedArgs[targetArgIndex] = bridgedArg;
            targetArgBitmask |= 1UL << targetArgIndex;

            return null;
        }

        // Resolves the params argment and fills in missing arguments.
        private BridgeCallFailure FinalizeArgumentBridging(
                MethodBase method,
                ParameterInfo[] parameters,
                object[] bridgedArgs,
                List<object> paramsArgs,
                ulong targetArgBitmask) {
            // Resolve the params argument, if there is one.
            if (paramsArgs != null) {
                Type elementType = parameters.Last().ParameterType.GetElementType();
                Array paramsArray = Array.CreateInstance(elementType, paramsArgs.Count());
                for (int i = 0; i < paramsArray.Length; i++)
                    paramsArray.SetValue(paramsArgs[i], i);

                bridgedArgs[bridgedArgs.Length - 1] = paramsArray;
            }

            // Process missing arguments. First, early out if there are no
            // missing arguments.
            if (targetArgBitmask == (1UL << parameters.Length) - 1UL)
                return null;

            // Fill in missing arguments with default values.
            for (int targetArgIndex = 0; targetArgIndex < parameters.Length; targetArgIndex++) {
                if ((targetArgBitmask & (1UL << targetArgIndex)) != 0UL)
                    continue;

                // We found a missing parameter. If it has no default value, we're out of luck.
                // Otherwise, patch in the parameter with its default value.
                ParameterInfo parameterInfo = parameters[targetArgIndex];
                if (!parameterInfo.HasDefaultValue)
                    return new BridgeCallArityMismatch(method);
                bridgedArgs[targetArgIndex] = parameterInfo.DefaultValue;
            }

            return null;
        }

        public override Evaluation Call(
                SchemeObject[] args, Env env, DynamicEnv dynEnv, bool allowDefine) {
            DynamicEnv oldDynEnv = SimpleNativeProc.CurrentDynamicEnv;
            SimpleNativeProc.CurrentDynamicEnv = dynEnv;

            try {
                // Try to call each overloaded method.
                List<BridgeCallFailure> failures = new List<BridgeCallFailure>();
                foreach (MethodBase methodInfo in mMethods) {
                    BridgeCallResult result = TryCall(methodInfo, args);
                    if (result is BridgeCallSuccess)
                        return new Result(((BridgeCallSuccess)result).ReturnValues);
                    failures.Add((BridgeCallFailure)result);
                }

                // If we got here, no methods matched. Report an error.
                StringBuilder error = new StringBuilder();
                error.AppendLine("No methods matched this call:");
                foreach (BridgeCallFailure failure in failures) {
                    error.AppendFormat(
                        "  Method {0} didn't match: {1}\n",
                        failure.Method,
                        failure.ToString(args));
                }
                throw new SchemeException("" + error);
            } finally {
                SimpleNativeProc.CurrentDynamicEnv = oldDynEnv;
            }
        }

        private abstract class BridgeCallResult { }

        private sealed class BridgeCallSuccess : BridgeCallResult {
            public SchemeObject[] ReturnValues;

            public BridgeCallSuccess(SchemeObject[] returnValues) {
                ReturnValues = returnValues;
            }
        }

        private abstract class BridgeCallFailure : BridgeCallResult {
            public MethodBase Method;

            protected BridgeCallFailure(MethodBase method) {
                Method = method;
            }

            public abstract string ToString(SchemeObject[] suppliedArgs);
        }

        private sealed class BridgeCallTypeMismatch : BridgeCallFailure {
            public int BadTargetArgIndex;
            public int BadSuppliedArgIndex;

            public BridgeCallTypeMismatch(
                    int badTargetArgIndex, int badSuppliedArgIndex, MethodBase method) :
                    base(method) {
                BadTargetArgIndex = badTargetArgIndex;
                BadSuppliedArgIndex = badSuppliedArgIndex;
            }

            public override string ToString(SchemeObject[] suppliedArgs) {
                ParameterInfo parameterInfo = Method.GetParameters()[BadTargetArgIndex];
                return String.Format(
                    "Argument of Scheme type {0} couldn't be converted to native type {1} in " +
                    "order to be passed to argument \"{2}\"",
                    suppliedArgs[BadSuppliedArgIndex].GetType(),
                    parameterInfo.ParameterType,
                    parameterInfo.Name);
            }
        }

        private sealed class BridgeCallArityMismatch : BridgeCallFailure {
            public BridgeCallArityMismatch(MethodBase method) : base(method) { }

            public override string ToString(SchemeObject[] suppliedArgs) {
                // FIXME: This could lead to bad error messages with params or optional args I
                // think.
                int paramCount = Method.GetParameters().Length;
                if (!Method.IsStatic)
                    paramCount++;

                return String.Format(
                    "This method takes {0} but {1} {2} supplied",
                    SExprUtils.Pluralize("argument", paramCount),
                    SExprUtils.Pluralize("argument", suppliedArgs.Length),
                    suppliedArgs.Length == 1 ? "was" : "were");
            }
        }

        private sealed class BridgeCallKeywordMismatch : BridgeCallFailure {
            public string Keyword;

            public BridgeCallKeywordMismatch(string keyword, MethodBase method) : base(method) {
                Keyword = keyword;
            }

            public override string ToString(SchemeObject[] suppliedArgs) {
                return String.Format("Method has no parameter named \"{0}\"", Keyword);
            }
        }

        private sealed class BridgeDuplicateArg : BridgeCallFailure {
            public string ArgName;

            public BridgeDuplicateArg(string argName, MethodBase method) : base(method) {
                ArgName = argName;
            }

            public override string ToString(SchemeObject[] suppliedArgs) {
                return String.Format("Parameter named \"{0}\" supplied multiple times", ArgName);
            }
        }

        private sealed class BridgeCallReceiverMismatch : BridgeCallFailure {
            public BridgeCallReceiverMismatch(MethodBase method) : base(method) { }

            public override string ToString(SchemeObject[] suppliedArgs) {
                return String.Format(
                    "Receiver of Scheme type {0} couldn't be converted to native type {1}",
                    suppliedArgs[0].GetType(),
                    Method.DeclaringType);
            }
        }
    }

    /// <summary>
    /// A native procedure that accesses a field.
    /// </summary>
    internal class BridgingAccessorProc : SimpleNativeProc {
        private FieldInfo mField;

        public BridgingAccessorProc(FieldInfo field) {
            mField = field;
        }

        protected override SchemeObject Call(SchemeObject[] args) {
            if (mField.IsStatic) {
                CheckArity(args, 0, 0);
                return SchemeObject.Bridge(mField.GetValue(null));
            }

            CheckArity(args, 1, 1);
            object target = args[0].BridgeTo(mField.DeclaringType);

            return SchemeObject.Bridge(mField.GetValue(target));
        }
    }

    /// <summary>
    /// A native procedure that mutates a field.
    /// </summary>
    internal class BridgingMutatorProc : SimpleNativeProc {
        private FieldInfo mField;

        public BridgingMutatorProc(FieldInfo field) {
            mField = field;
        }

        protected override SchemeObject Call(SchemeObject[] args) {
            if (mField.IsStatic) {
                CheckArity(args, 1, 1);
                object value = args[1].BridgeTo(mField.FieldType);

                mField.SetValue(null, value);
            } else {
                CheckArity(args, 2, 2);
                object target = args[0].BridgeTo(mField.DeclaringType);
                object value = args[1].BridgeTo(mField.DeclaringType);

                mField.SetValue(target, value);
            }

            return SchemeNull.Null;
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
