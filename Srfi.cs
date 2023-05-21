// SmolScheme/Srfi88.cs

// SRFI 88: Keyword objects.
//
// https://srfi.schemers.org/srfi-88/srfi-88.html

using System.Collections.Generic;

namespace SmolScheme.Srfi88 {
    public static class KeywordLibrary {
        public static Library Create() {
            Library library = new Library();
            library.AddBridgedClass(typeof(Keyword));
            return library;
        }
    }

    [SchemeBridge.TypeTest]
    public class Keyword : SchemeObject {
        public readonly string Name;

        public Keyword(string name) {
            Name = name;
        }

        [SchemeBridge("String->Keyword")]
        public static Keyword FromSRepr(string sRepr) {
            return new Keyword(Identifier.SwitchRepr(sRepr));
        }

        public override bool Eqv(SchemeObject other) {
            return other is Keyword && Name == ((Keyword)other).Name;
        }

        public override string StructureToString(ToStringOptions options) {
            // TODO: Skip parentheses and next comma when inside a function call.
            return "(" + Name + ":)";
        }

        [SchemeBridge("Keyword->String")]
        public static string KeywordToString(Keyword keyword) {
            return keyword.StructureToString(new ToStringOptions((ToStringFlags)0));
        }

        public string SRepr {
            get { return Identifier.SwitchRepr(Name); }
        }
    }
}

