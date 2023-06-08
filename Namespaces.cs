// SmolScheme/Namespaces.cs

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;

namespace SmolScheme {
    // 5 Program structure

    // 5.2 Import declarations

    public abstract class ImportSet {
        public readonly Dictionary<string, Def> Imports;

        protected ImportSet(Dictionary<string, Def> imports) {
            Imports = imports;
        }

        protected ImportSet() : this(new Dictionary<string, Def>()) { }

        protected void AddImport(string import, Def def) {
            if (Imports.ContainsKey(import)) {
                throw new ImportException(String.Format(
                    "A symbol named `{0}` has already been imported",
                    Interpreter.Current.FormatSymbol(import)));
            }

            Imports.Add(import, def);
        }
    }

    public class LibraryImportSet : ImportSet {
        public LibraryImportSet(Library library) : base(library.Exports) { }
    }

    public class OnlyImportSet : ImportSet {
        public OnlyImportSet(ImportSet target, string[] subset) : base() {
            foreach (string import in subset) {
                if (!target.Imports.ContainsKey(import)) {
                    throw new ImportException(String.Format(
                        "Symbol `{0}` wasn't found in the original import set",
                        Interpreter.Current.FormatSymbol(import)));
                }

                AddImport(import, target.Imports[import]);
            }
        }
    }

    public class ExceptImportSet : ImportSet {
        public ExceptImportSet(ImportSet target, HashSet<string> exclusions) : base() {
            foreach (KeyValuePair<string, Def> pair in target.Imports) {
                if (exclusions.Contains(pair.Key))
                    exclusions.Remove(pair.Key);
                else
                    AddImport(pair.Key, pair.Value);
            }

            if (exclusions.Count > 0) {
                throw new ImportException(
                    "Symbols " + String.Join(", ", exclusions.Select(str => "`" + str + "`")) +
                    " weren't found in the original import set");
            }
        }
    }

    public class RenameImportSet : ImportSet {
        public RenameImportSet(ImportSet target, Dictionary<string, string> renamings) : base() {
            foreach (KeyValuePair<string, Def> pair in target.Imports) {
                if (renamings.ContainsKey(pair.Key)) {
                    AddImport(renamings[pair.Key], pair.Value);
                    renamings.Remove(pair.Key);
                } else {
                    AddImport(pair.Key, pair.Value);
                }
            }

            if (renamings.Count > 0) {
                throw new ImportException(
                    "Symbols " +
                    String.Join(", ", renamings.Select(pair => "`" + pair.Key + "`")) +
                    " weren't found in the original import set");
            }
        }
    }

    public class PrefixImportSet : ImportSet {
        public PrefixImportSet(ImportSet target, string prefix) : base() {
            prefix = Identifier.SwitchRepr(prefix);
            foreach (KeyValuePair<string, Def> pair in target.Imports) {
                AddImport(Identifier.SwitchRepr(prefix + Identifier.SwitchRepr(pair.Key)),
                    pair.Value);
            }
        }
    }

    public class ImportException : SchemeException {
        internal ImportException(string message) : base(message) { }
    }

    internal class ImportForm : Syntax {
        // This is factored out so that library declaration syntax can use it too.
        internal static void Import(ListExpr listExpr, Env env) {
            SExpr[] args = GetArguments(listExpr);

            foreach (SExpr arg in args) {
                ImportSet importSet = Interpreter.Current.Libraries.CreateImportSet(arg);
                foreach (KeyValuePair<string, Def> import in importSet.Imports)
                    env.Add(import.Key, import.Value);
            }
        }

        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                Mode mode) {
            if ((mode & Mode.AllowImports) == 0)
                throw new SchemeException("Imports are only allowed at the start of the program");

            Import(listExpr, env);
            return new Result(SchemeNull.Null);
        }
    }

    // 5.3 Variable definitions

    internal class DefineForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                Mode mode) {
            if ((mode & Mode.AllowDefine) == 0) {
                throw new SchemeException(
                    "Define is only allowed at top-level or in bodies",
                    listExpr.Loc);
            }

            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 2);
            SExpr firstExpr = args[0], secondExpr = args[1];

            string name;
            SchemeObject value;
            if (firstExpr is Identifier) {
                // Variable form.
                name = ((Identifier)firstExpr).Name;
                value = Interpreter.Current.Evaluate(secondExpr, env, dynEnv);
            } else if (firstExpr is ListExpr) {
                // Normal function definition form.
                var sigExpr = (ListExpr)firstExpr;
                if (!(sigExpr.Head is Identifier))
                    throw new SchemeException("The function name must be a symbol");
                name = ((Identifier)sigExpr.Head).Name;
                value = new SchemeProcDatum(
                    CaseLambdaForm.BuildClause(sigExpr.GetTailExpr(), secondExpr), env);
            } else if (firstExpr is SplatExpr) {
                var sigExpr = (SplatExpr)firstExpr;
                if (!(sigExpr.Head is Identifier))
                    throw new SchemeException("The function name must be a symbol");
                if (!(sigExpr.Tail is Identifier))
                    throw new SchemeException("The name for the variadic argument must be a symbol");
                name = ((Identifier)sigExpr.Head).Name;
                value = new SchemeProcDatum(CaseLambdaForm.BuildClause(sigExpr.Tail, secondExpr), env);
            } else {
                throw new SchemeException(
                    "The first argment to `Define` must be a symbol, list, or pair");
            }

            env.Add(name, value);
            return new Result(SchemeNull.Null);
        }
    }

    // 5.3.3 Multiple-value definitions

    internal class DefineValuesForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                Mode mode) {
            if ((mode & Mode.AllowDefine) == 0) {
                throw new SchemeException(
                    "Define is only allowed at top-level or in bodies",
                    listExpr.Loc);
            }

            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 2);
            SExpr formalsExpr = args[0], valueExpr = args[1];

            SExpr[] formals = formalsExpr.RequireListExpr().ToArray();
            string[] names = formals.Select(formal => formal.RequireIdentifier()).ToArray();

            SchemeObject[] values = Interpreter.Current.EvaluateMulti(
                valueExpr, env, dynEnv, mode & ~Mode.AllowDefine, names.Length);

            for (int i = 0; i < names.Length; i++)
                env.Add(names[i], values[i]);

            return new Result(SchemeNull.Null);
        }
    }

    // 5.5 Record-type definitions

    internal class DefineRecordTypeForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                Mode mode) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 3, -1);

            string recordName = args[0].RequireIdentifier();

            SExpr[] constructorExpr = args[1].RequireListExpr().ToArray();
            int fieldCount = args.Length - 3;
            if (constructorExpr.Length != fieldCount + 1) {
                throw new SchemeException("The constructor signature must have as many " +
                    "arguments as there are fields");
            }
            string constructorName = constructorExpr[0].RequireIdentifier();

            var fieldNames = new string[fieldCount];
            var fieldAccessors = new string[fieldCount];
            var fieldMutators = new string[fieldCount];

            for (int i = 0; i < fieldCount; i++) {
                SExpr[] fields = args[i + 3].RequireListExpr().ToArray();
                if (fields.Length < 2 || fields.Length > 3) {
                    throw new SchemeException("The field specification must have either 2 or 3 " +
                        "elements");
                }
                fieldNames[i] = fields[0].RequireIdentifier();
                fieldAccessors[i] = fields[1].RequireIdentifier();
                fieldMutators[i] = fields.Length < 3 ? null : fields[2].RequireIdentifier();
            }

            string predicateName = args[2].RequireIdentifier();

            var recordType = new RecordType(recordName, constructorName, fieldNames);

            env.Add(constructorName, new MakeRecordProc(recordType));
            env.Add(predicateName, new IsRecordProc(recordType));

            for (int i = 0; i < fieldCount; i++) {
                env.Add(fieldAccessors[i], new RecordGetProc(recordType, i));
                if (fieldMutators[i] != null)
                    env.Add(fieldMutators[i], new RecordSetProc(recordType, i));
            }

            return new Result(SchemeNull.Null);
        }
    }

    // 5.6 Libraries

    public class Libraries {
        private readonly Dictionary<LibraryName, Library> mLoadedLibraries;

        /// <summary>
        /// The library search path, for `.sld` files.
        /// </summary>
        private readonly List<string> mSearchPath;

        /// <summary>
        /// Whether programs should be able to import CLR classes under the `Clr` namespace.
        /// </summary>
        private readonly bool mAllowClr;

        public Libraries(bool allowClr = true) {
            mAllowClr = allowClr;

            mLoadedLibraries = new Dictionary<LibraryName, Library> {
                { new LibraryName("Scheme", "Base"), BaseLibrary.Create() },
                { new LibraryName("Scheme", "CaseLambda"), CaseLambdaLibrary.Create() },
                { new LibraryName("Scheme", "Char"), CharLibrary.Create() },
                { new LibraryName("Scheme", "Complex"), ComplexLibrary.Create() },
                { new LibraryName("Scheme", "Cxr"), CxrLibrary.Create() },
                { new LibraryName("Scheme", "Eval"), EvalLibrary.Create() },
                { new LibraryName("Scheme", "File"), FileLibrary.Create() },
                { new LibraryName("Scheme", "Inexact"), InexactLibrary.Create() },
                { new LibraryName("Scheme", "Lazy"), LazyLibrary.Create() },
                { new LibraryName("Scheme", "Load"), SmolScheme.LoadLibrary.Create() },
                { new LibraryName("Scheme", "ProcessContext"), ProcessContextLibrary.Create() },
                { new LibraryName("Scheme", "Read"), ReadLibrary.Create() },
                { new LibraryName("Scheme", "Repl"), ReplLibrary.Create() },
                { new LibraryName("Scheme", "Time"), TimeLibrary.Create() },
                { new LibraryName("Scheme", "Write"), WriteLibrary.Create() },
            };

            Library r5rsLibrary = R5RSLibrary.Create(mLoadedLibraries.Select(pair => pair.Value));
            mLoadedLibraries.Add(new LibraryName("Scheme", "R5rs"), r5rsLibrary);
            mLoadedLibraries.Add(new LibraryName("Srfi", "88"), Srfi88.KeywordLibrary.Create());
            mLoadedLibraries.Add(new LibraryName("SmolScheme", "Extensions"),
                SmolSchemeExtensionsLibrary.Create());
        }

        /// <summary>
        /// The library search path, for `.sld` files.
        /// </summary>
        public IEnumerable<string> SearchPath {
            get { return mSearchPath; }
        }

        public ImportSet CreateImportSet(SExpr expr) {
            if (!(expr is ListExpr))
                throw new ImportException("Each import set must be a list");

            ListExpr listExpr = (ListExpr)expr;
            IEnumerable<SExpr> args = listExpr.RequireListExpr(2, -1);

            string form = args.First().RequireIdentifier();

            if (form == "Only") {
                ImportSet target = CreateImportSet(args.Skip(1).First());
                string[] only = args.Skip(2).Select(arg => arg.RequireIdentifier()).ToArray();
                return new OnlyImportSet(target, only);
            }

            if (form == "Except") {
                ImportSet target = CreateImportSet(args.Skip(1).First());
                IEnumerable<string> except = args.Skip(2).Select(arg => arg.RequireIdentifier());
                return new ExceptImportSet(target, new HashSet<string>(except));
            }

            if (form == "Prefix") {
                ImportSet target = CreateImportSet(args.Skip(1).First());
                SExpr[] argsArray = args.ToArray();
                if (argsArray.Length != 3)
                    throw new ImportException("The \"Prefix\" import form must have 2 arguments");
                return new PrefixImportSet(target, argsArray[2].RequireIdentifier());
            }

            if (form == "Rename") {
                ImportSet target = CreateImportSet(args.Skip(1).First());
                var renamings = new Dictionary<string, string>();
                foreach (SExpr renaming in args.Skip(2)) {
                    SExpr[] renamingParts = renaming.RequireListExpr(2, 2).ToArray();
                    string renameFrom = renamingParts[0].RequireIdentifier();
                    string renameTo = renamingParts[1].RequireIdentifier();
                    renamings.Add(renameFrom, renameTo);
                }
                return new RenameImportSet(target, renamings);
            }

            return new LibraryImportSet(RequireLibrary(new LibraryName(args)));
        }

        public Library LoadLibrary(LibraryName libraryName) {
            // Have we already loaded the library?
            if (mLoadedLibraries.ContainsKey(libraryName))
                return mLoadedLibraries[libraryName];

            // Try to load a CLR library.
            if (mAllowClr && libraryName[0] == "Clr")
                return LoadClrLibrary(libraryName);

            // Look for the library on disk.
            string libraryPath = libraryName.ToPath();
            foreach (string searchDir in SearchPath) {
                string fullPath = Path.Combine(searchDir, libraryPath);
                try {
                    using (FileStream stream = File.Open(fullPath, FileMode.Open)) {
                        Interpreter.Current.Load(
                            stream,
                            new Env(),
                            SimpleNativeProc.CurrentDynamicEnv, fullPath);

                        if (mLoadedLibraries.ContainsKey(libraryName))
                            return mLoadedLibraries[libraryName];
                    }
                } catch (DirectoryNotFoundException) {
                } catch (FileNotFoundException) {
                }
            }

            return null;
        }

        private Library LoadClrLibrary(LibraryName libraryName) {
            string[] segments = libraryName.Segments.Skip(1).ToArray();

            // Search inner classes by moving a `+` back through the list of supplied names.
            Type type = null;
            for (int innerClassPos = segments.Length - 1;
                    type == null && innerClassPos >= 0;
                    innerClassPos--) {
                StringBuilder typeNameBuilder = new StringBuilder();
                for (int i = 0; i < segments.Length; i++) {
                    typeNameBuilder.Append(segments[i]);
                    if (i < segments.Length - 1)
                        typeNameBuilder.Append(i == innerClassPos ? '+' : '.');
                }
                string typeName = "" + typeNameBuilder;

                foreach (Assembly assembly in AppDomain.CurrentDomain.GetAssemblies()) {
                    if ((type = assembly.GetType(typeName)) != null)
                        break;
                }
            }
            if (type == null)
                return null;

            Library library = new Library();
            library.AddBridgedClass(type, true);
            Add(libraryName, library);
            return library;
        }

        public Library RequireLibrary(LibraryName libraryName) {
            Library library = LoadLibrary(libraryName);
            if (library != null)
                return library;

            throw new ImportException(String.Format(
                "No library named \"{0}\" was found", String.Join(", ", libraryName.Segments)));
        }

        public Library RequireLibrary(params string[] libraryName) {
            return RequireLibrary(new LibraryName(libraryName));
        }

        public Env CreateInteractionEnvironment() {
            Env rib = CreateNullEnvironment();
            rib.Import(RequireLibrary("Scheme", "Base"));
            rib.Import(RequireLibrary("Scheme", "CaseLambda"));
            rib.Import(RequireLibrary("Scheme", "Char"));
            rib.Import(RequireLibrary("Scheme", "Complex"));
            rib.Import(RequireLibrary("Scheme", "Cxr"));
            rib.Import(RequireLibrary("Scheme", "Eval"));
            rib.Import(RequireLibrary("Scheme", "File"));
            rib.Import(RequireLibrary("Scheme", "Inexact"));
            rib.Import(RequireLibrary("Scheme", "Lazy"));
            rib.Import(RequireLibrary("Scheme", "Load"));
            rib.Import(RequireLibrary("Scheme", "ProcessContext"));
            rib.Import(RequireLibrary("Scheme", "Read"));
            rib.Import(RequireLibrary("Scheme", "Repl"));
            rib.Import(RequireLibrary("Scheme", "Time"));
            rib.Import(RequireLibrary("Scheme", "Write"));
            rib.Import(RequireLibrary("Srfi", "88"));
            rib.Import(RequireLibrary("SmolScheme", "Extensions"));
            return rib;
        }

        /// <summary>
        /// Creates the environment that source files begin with, containing enough definitions to
        /// bootstrap.
        /// </summary>
        public Env CreateNullEnvironment() {
            return new Env() {
                {"DefineLibrary", new SyntaxDef(new DefineLibraryForm())},
                {"Import", new SyntaxDef(new ImportForm())},
            };
        }

        public Env CreateR5RSEnvironment() {
            var rib = new Env();
            rib.Import(RequireLibrary("Scheme", "R5rs"));
            return rib;
        }

        public Env CreateNullR5RSEnvironment() {
            var rib = new Env();
            rib.Import(
                new OnlyImportSet(
                    new LibraryImportSet(RequireLibrary("Scheme", "R5rs")),
                    new string[] {
                        "...",
                        "_",
                        "Begin",
                        "Case",
                        "Define",
                        "DefineSyntax",
                        "Delay",
                        "Do",
                        "Else",
                        "If",
                        "Lambda",
                        "Let",
                        "Let*",
                        "LetSyntax",
                        "Letrec",
                        "LetrecSyntax",
                        "Quasiquote",
                        "Quote",
                        "Set!",
                        "SyntaxRules",
                    }));
            return rib;
        }

        public void Add(LibraryName name, Library library) {
            if (mLoadedLibraries.ContainsKey(name)) {
                throw new SchemeException(String.Format(
                    "A library named \"{0}\" is already registered", name));
            }

            mLoadedLibraries.Add(name, library);
        }
    }

    public class Library {
        public readonly Dictionary<string, Def> Exports;

        public Library(Dictionary<string, Def> exports) {
            Exports = exports;
        }

        public Library() : this(new Dictionary<string, Def>()) {}

        public void AddBridgedClass(Type type, bool addAll = false) {
            AddBridgedMethods(type, addAll);
            AddBridgedFields(type, addAll);
            AddBridgedTypeTest(type, addAll);
        }

        private string GetBridgeName(
                bool alwaysAdd, Func<SchemeBridgeAttribute> getAttr, string defaultName) {
            string name = defaultName;

            SchemeBridgeAttribute schemeBridge = getAttr();
            if (schemeBridge == null && !alwaysAdd)
                return null;
            if (schemeBridge != null && schemeBridge.Name != null)
                name = schemeBridge.Name;

            return name;
        }

        private void AddBridgedMethods(Type type, bool addAll) {
            List<MethodBase> methods = new List<MethodBase>();
            methods.AddRange(type.GetMethods());
            methods.AddRange(type.GetConstructors());

            // Index all properties for quick access.
            // TODO: Maybe populate this lazily?
            var accessors = new Dictionary<MethodInfo, PropertyInfo>();
            var mutators = new Dictionary<MethodInfo, PropertyInfo>();
            foreach (PropertyInfo property in type.GetProperties()) {
                MethodInfo accessor = property.GetGetMethod();
                if (accessor != null)
                    accessors.Add(accessor, property);

                MethodInfo mutator = property.GetSetMethod();
                if (mutator != null)
                    mutators.Add(mutator, property);
            }

            var bridgedMethods = new Dictionary<string, List<MethodBase>>();
            foreach (MethodBase method in methods) {
                string defaultName;
                if (method is MethodInfo) {
                    // Check to see if this method is a property.
                    MethodInfo methodInfo = (MethodInfo)method;
                    if (accessors.ContainsKey(methodInfo))
                        defaultName = accessors[methodInfo].Name;
                    else if (mutators.ContainsKey(methodInfo))
                        defaultName = "Set" + mutators[methodInfo].Name + "!";
                    else
                        defaultName = method.Name;
                } else {
                    defaultName = type.Name;
                }

                string name = GetBridgeName(addAll && method.IsPublic, () => {
                        // Don't inherit attributes from superclasses.
                        if (method.DeclaringType == type)
                            return method.GetCustomAttribute<SchemeBridge>(false);
                        return null;
                    }, defaultName);
                if (name == null)
                    continue;

                if (!bridgedMethods.ContainsKey(name))
                    bridgedMethods.Add(name, new List<MethodBase>());
                bridgedMethods[name].Add(method);
            }

            foreach (KeyValuePair<string, List<MethodBase>> pair in bridgedMethods)
                Exports.Add(pair.Key, new ObjectDef(new BridgingNativeProc(pair.Value.ToArray())));
        }

        private void AddBridgedFields(Type type, bool addAll) {
            var bridgedAccessors = new Dictionary<string, FieldInfo>();
            var bridgedMutators = new Dictionary<string, FieldInfo>();
            var bridgedStaticConsts = new Dictionary<string, object>();

            foreach (FieldInfo field in type.GetFields()) {
                string accessorName = GetBridgeName(addAll && field.IsPublic, () => {
                    // Ignore this attribute on subclasses. Otherwise, name collisions are way too
                    // easy to come by.
                    if (field.DeclaringType != type)
                        return null;

                    SchemeBridgeAttribute accessorAttr =
                        field.GetCustomAttribute<SchemeBridge.Accessor>(false);
                    SchemeBridgeAttribute mainAttr = field.GetCustomAttribute<SchemeBridge>(false);
                    if (accessorAttr != null) {
                        if (mainAttr != null) {
                            throw new SchemeException("Can't specify both SchemeBridge and " +
                                "SchemeBridge.Accessor on a field");
                        }
                        return accessorAttr;
                    }
                    return mainAttr;
                }, field.Name);

                string mutatorName;
                if (field.IsInitOnly) {
                    if (field.GetCustomAttribute<SchemeBridge.Mutator>(false) != null) {
                        throw new SchemeException("Can't specify a mutator for this field as " +
                            "it's read-only");
                    }
                    mutatorName = null;
                } else {
                    mutatorName = GetBridgeName(addAll && field.IsPublic, () => {
                        // Ignore this attribute on subclasses. Otherwise, name collisions are way
                        // too easy to come by.
                        if (field.DeclaringType != type)
                            return null;

                        SchemeBridgeAttribute mutatorAttr =
                            field.GetCustomAttribute<SchemeBridge.Mutator>(false);
                        SchemeBridgeAttribute mainAttr =
                            field.GetCustomAttribute<SchemeBridge>(false);
                        if (mutatorAttr != null) {
                            if (mainAttr != null) {
                                throw new SchemeException("Can't specify both SchemeBridge and " +
                                    "SchemeBridge.Mutator on a field");
                            }
                            return mutatorAttr;
                        }
                        return mainAttr;
                    }, field.Name);
                }

                if (accessorName != null) {
                    if (field.IsStatic && field.IsInitOnly)
                        bridgedStaticConsts.Add(accessorName, field.GetValue(null));
                    else
                        bridgedAccessors.Add(accessorName, field);
                }

                if (mutatorName != null) {
                    if (field.GetCustomAttribute<SchemeBridge.Mutator>(false) == null)
                        mutatorName = String.Format("Set{0}!", mutatorName);
                    bridgedMutators.Add(mutatorName, field);
                }
            }

            foreach (KeyValuePair<string, FieldInfo> pair in bridgedAccessors)
                Exports.Add(pair.Key, new ObjectDef(new BridgingAccessorProc(pair.Value)));
            foreach (KeyValuePair<string, FieldInfo> pair in bridgedMutators)
                Exports.Add(pair.Key, new ObjectDef(new BridgingMutatorProc(pair.Value)));
            foreach (KeyValuePair<string, object> pair in bridgedStaticConsts)
                Exports.Add(pair.Key, new ObjectDef(SchemeObject.Bridge(pair.Value)));
        }

        private void AddBridgedTypeTest(Type type, bool addAll) {
            string name = GetBridgeName(addAll,
                () => type.GetCustomAttribute<SchemeBridge.TypeTest>(false), type.Name + "?");
            if (name == null)
                return;

            // Static classes have no type test.
            if (type.IsAbstract && type.IsSealed) {
                if (type.GetCustomAttribute<SchemeBridge.TypeTest>(false) != null)
                    throw new SchemeException("Can't make a type test for a static class");
                return;
            }

            SchemeProc proc;
            if (typeof(SchemeObject).IsAssignableFrom(type))
                proc = new IsTypeProc(type);
            else
                proc = new IsNativeTypeProc(type);

            Exports.Add(name, new ObjectDef(proc));
        }
    }

    public class LibraryName {
        public readonly string[] Segments;

        public LibraryName(params string[] segments) {
            Segments = segments;
        }

        public LibraryName(IEnumerable<string> segments) : this(segments.ToArray()) { }

        public LibraryName(IEnumerable<SExpr> segments) {
            Segments = segments.Select(segment => segment.RequireIdentifier()).ToArray();
        }

        public LibraryName(ListExpr expr) : this(expr.RequireListExpr()) { }

        public string this[int i] {
            get {
                return Segments[i];
            }
        }

        public int Length {
            get { return Segments.Length; }
        }

        public override bool Equals(object obj) {
            if (obj == null || GetType() != obj.GetType())
                return false;

            LibraryName other = (LibraryName)obj;
            if (Length != other.Length)
                return false;
            for (int i = 0; i < other.Length; i++) {
                if (this[i] != other[i])
                    return false;
            }

            return true;
        }

        public override int GetHashCode() {
            // FNV-1a.
            // https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
            uint hash = 2166136261;
            foreach (string segment in Segments)
                hash = (hash ^ (uint)segment.GetHashCode()) * 16777619;
            return (int)hash;
        }

        public string ToPath() {
            return Path.Combine(Segments);
        }
    }

    // 5.4 Syntax definitions

    internal class DefineSyntaxForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                Mode mode) {
            if ((mode & Mode.AllowDefine) == 0)
                throw new SchemeException("DefineSyntax isn't allowed here");

            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 2);
            string name = args[0].RequireIdentifier();
            ListExpr syntaxRulesExpr = args[1].Require<ListExpr>();

            if (!env.RefersTo(syntaxRulesExpr.Head, SyntaxRulesMacro.SyntaxRules)) {
                throw new SchemeException(
                    "The second argument to DefineSyntax must be SyntaxRules[...]");
            }

            env.Add(name, new SyntaxDef(new SyntaxRulesMacro(syntaxRulesExpr, env)));
            return new Result();
        }
    }

    // 5.6 Libraries

    // 5.6.1 Library syntax

    internal class DefineLibraryForm : Syntax {
        public override Evaluation Evaluate(
                ListExpr listExpr,
                Env env,
                DynamicEnv dynEnv,
                Mode mode) {
            SExpr[] args = GetArguments(listExpr);
            CheckArity(args, 1, -1);

            LibraryName libraryName = new LibraryName(args[0].Require<ListExpr>());

            var exportExprs = new List<ListExpr>();
            var bodyExprs = new List<ListExpr>();
            Env bodyEnv = new Env();

            // Process declarations.
            // We reverse this list for efficiency.
            List<SExpr> decls = args.Skip(1).Reverse().ToList();
            while (decls.Count > 0)
                ProcessDecl(decls, exportExprs, bodyExprs, bodyEnv);

            // Evaluate the body.
            foreach (ListExpr bodyExpr in bodyExprs)
                Interpreter.Current.EvaluateMulti(bodyExpr, bodyEnv, dynEnv, Mode.AllowDefine);

            // Process exports.
            var exports = new Dictionary<string, Def>();
            foreach (ListExpr exportExpr in exportExprs) {
                foreach (SExpr exportSpecExpr in exportExpr.Tail)
                    ProcessExport(exportSpecExpr, exports, bodyEnv);
            }

            // Build the library.
            Library newLibrary = new Library(exports);
            Interpreter.Current.Libraries.Add(libraryName, newLibrary);
            return new Result();
        }

        private void ProcessDecl(
                List<SExpr> decls,
                List<ListExpr> exportExprs,
                List<ListExpr> bodyExprs,
                Env bodyEnv) {
            ListExpr declExpr = decls.Last().Require<ListExpr>();
            decls.RemoveAt(decls.Count - 1);

            IEnumerable<SExpr> decl = declExpr.RequireListExpr(1, -1);
            string declType = decl.First().RequireIdentifier();

            // Save some declarations for later.
            if (declType == "Export") {
                exportExprs.Add(declExpr);
                return;
            }
            if (declType == "Begin" || declType == "Include" || declType == "IncludeCi") {
                bodyExprs.Add(declExpr);
                return;
            }

            // Process imports now. (This is an alternate semantics allowed by the standard.)
            if (declType == "Import") {
                ImportForm.Import(declExpr, bodyEnv);
                return;
            }

            if (declType == "IncludeLibraryDeclarations") {
                SExpr[] newDecls = IncludeForm.Include(declExpr, false);
                foreach (SExpr newDecl in newDecls.Reverse())
                    decls.Add(newDecl);
                return;
            }

            // Handle `CondExpand`.
            if (declType == "CondExpand") {
                SourceLocation loc;

                Env env = new Env();
                env.Add("Else", CondForm.Else);

                IEnumerable<SExpr> expanded = CondExpandForm.EvaluateForm(declExpr, env, out loc);
                if (expanded != null)
                    decls.AddRange(expanded.Reverse());
                return;
            }

            throw new SchemeException(
                "Expected `Export`, `Import`, `Begin`, `Include`, `IncludeCi`, " +
                "`IncludeLibraryDeclarations`, or `CondExpand` but found `" + declType + "`");
        }

        private void ProcessExport(
                SExpr exportSpecExpr,
                Dictionary<string, Def> exports,
                Env bodyEnv) {
            string destName, srcName;
            if (exportSpecExpr is Identifier) {
                destName = srcName = ((Identifier)exportSpecExpr).Name;
            } else if (exportSpecExpr is ListExpr) {
                ListExpr exportSpecListExpr = (ListExpr)exportSpecExpr;
                string opName = exportSpecListExpr.Head.RequireIdentifier();
                if (opName != "Rename") {
                    throw new SchemeException(
                        "Expected \"Rename\" but found \"" + opName + "\"");
                }

                SExpr[] exportSpecArgs = GetArguments(exportSpecListExpr);
                CheckArity(exportSpecArgs, 2, 2);
                srcName = exportSpecArgs[0].RequireIdentifier();
                destName = exportSpecArgs[1].RequireIdentifier();
            } else {
                throw new SchemeException("Expected symbol or list");
            }

            Def def = bodyEnv.Get(srcName);
            if (def == null) {
                throw new SchemeException("\"" + srcName + "\" wasn't " +
                    "exported by the library");
            }
            exports.Add(destName, def);
        }
    }
}
