// SmolScheme/SmolScheme.cs

using Mono.Options;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using SmolScheme;

namespace SmolScheme {
    // Main

    public class Shell : IAutoCompleteHandler {
        private char[] mSeparators;
        private Interpreter mInterpreter;
        private Env mEnv;

        private Shell() {
            mSeparators = "[](){}".ToCharArray();

            mInterpreter = new Interpreter(new Libraries());
            mEnv = mInterpreter.Libraries.CreateInteractionEnvironment();
        }

        public char[] Separators {
            get { return mSeparators; }
            set { mSeparators = value; }
        }

        public string[] GetSuggestions(string text, int index) {
            // You might think that you would want to sort this array and then use binary search,
            // but that ends up being slower than just O(n) linear search because sorting is
            // O(n log n).
            var candidates = new List<string>();
            foreach (string symbol in mEnv.Symbols) {
                if (symbol.StartsWith(text))
                    candidates.Add(symbol);
            }
            candidates.Sort();
            return candidates.ToArray();
        }

        private void Run(string[] args) {
            bool useMJSReader = false;
            OptionSet options = new OptionSet() {
                { "m|mjs", m => useMJSReader = true }
            };
            List<string> remainingArgs = options.Parse(args);

            DynamicEnv dynEnv = new DynamicEnv();
            dynEnv.OpenDefaultIOPorts();

            if (remainingArgs.Count == 1) {
                mInterpreter.Load(mEnv, dynEnv, remainingArgs[0], useMJSReader);
                return;
            }

            ReadLine.AutoCompletionHandler = this;

            while (true) {
                try {
                    string line = ReadLine.Read(">>> ");
                    if (line == null)
                        break;

                    using (var textReader = new StreamReader(new BufferedStream(new MemoryStream(
                            Encoding.UTF8.GetBytes(line))))) {
                        Reader reader;
                        if (useMJSReader)
                            reader = new MJSReader(textReader);
                        else
                            reader = new SReader(textReader);

                        try {
                            SExpr expr = reader.ParseSExpr();
                            SchemeObject[] datums = mInterpreter.EvaluateMulti(
                                expr, mEnv, dynEnv, true);
                            foreach (SchemeObject datum in datums)
                                Console.WriteLine(datum);
                        } finally {
                            reader.Dispose();
                        }
                    }
                } catch (SchemeException exception) {
                    Console.WriteLine("Exception: " + exception);
                }
            }
        }

        public static void Main(string[] args) {
            new Shell().Run(args);
        }
    }
}
