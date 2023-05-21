// SmolScheme/List.cs
//
// Lists.

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
    [SchemeBridge.TypeTest("Pair?")]
    public class SchemeList : SchemeObject, IEnumerable<SchemeObject> {
        [SchemeBridge.Accessor("Car")]
        [SchemeBridge.Mutator("SetCar!")]
        public SchemeObject Head;

        [SchemeBridge.Accessor("Cdr")]
        [SchemeBridge.Mutator("SetCdr!")]
        public SchemeObject Tail;

        public IEnumerable<SchemeObject> TailElements {
            get {
                return Tail is SchemeList ? (SchemeList)Tail : null;
            }
        }

        [SchemeBridge("Cons")]
        public SchemeList(SchemeObject head, SchemeObject tail) : base() {
            Head = head;
            Tail = tail;
        }

        public SchemeList(SchemeObject head) : this(head, SchemeNull.Null) { }

        public SchemeList(SchemeObject head, IEnumerable<SchemeObject> rest) :
                this(head, rest, SchemeNull.Null) { }

        public SchemeList(SchemeObject head, IEnumerable<SchemeObject> rest, SchemeObject tail)
                : base() {
            Head = head;
            // TODO: Make this more efficient...
            Tail = rest.Any() ? new SchemeList(rest.First(), rest.Skip(1)) : tail;
        }

        public static SchemeObject CreateList(IEnumerable<SchemeObject> elements) {
            if (!elements.Any())
                return SchemeNull.Null;
            return new SchemeList(elements.First(), elements.Skip(1));
        }

        [SchemeBridge("MakeList")]
        public static SchemeObject CreateList(int k, SchemeObject fill) {
            return CreateList(Enumerable.Repeat(fill, k));
        }

        [SchemeBridge("List")]
        public static SchemeObject CreateListFromArray(params SchemeObject[] elements) {
            return CreateList(elements);
        }

        public override bool Eqv(SchemeObject other) {
            return this == other;
        }

        IEnumerator IEnumerable.GetEnumerator() {
            return ((IEnumerable<SchemeObject>)this).GetEnumerator();
        }

        IEnumerator<SchemeObject> IEnumerable<SchemeObject>.GetEnumerator() {
            return new ListEnumerator(this);
        }

        public SchemeObject this[int i] {
            get { return this.Skip(i).First(); }
        }

        public static SchemeObject Append(SchemeObject self, SchemeObject other) {
            if (other is SchemeNull)
                return self;
            if (self is SchemeNull)
                return other;
            SchemeList list = (SchemeList)self;
            return new SchemeList(list.Head, Append(list.Tail, other));
        }

        public override string StructureToString(ToStringOptions options) {
            if (options.ShouldBeQuotedButIsnt) {
                return "'" + StructureToString(options.DuplicateWithFlags(
                    ToStringFlags.ShouldBeQuoted | ToStringFlags.AlreadyQuoted));
            }

            StringBuilder builder = new StringBuilder();

            bool useDot = !(Tail is SchemeNull || Tail is SchemeList) ||
                    options.Visited.Contains(Tail);

            if (options.ContainsFlags(ToStringFlags.PartOfList))
                builder.Append(", ");
            else
                builder.Append("(");

            ToStringOptions tailOptions;
            if (useDot)
                tailOptions = options;
            else
                tailOptions = options.DuplicateWithFlags(ToStringFlags.PartOfList);

            builder.AppendFormat(
                "{0}{1}{2}",
                Head.ToString(options),
                useDot ? " . " : "",
                Tail.ToString(tailOptions));

            if (!options.ContainsFlags(ToStringFlags.PartOfList))
                builder.Append(")");

            return "" + builder;
        }

        internal override SExpr Unquote() {
            var elements = new List<SExpr>();
            SchemeList current = this;

            while (true) {
                elements.Add(current.Head.Unquote());
                if (current.Tail is SchemeNull)
                    return new ListExpr(elements, Loc);
                if (current.Tail is SchemeList) {
                    current = (SchemeList)current.Tail;
                    continue;
                }
                return new SplatExpr(
                    elements.First(), elements.Skip(1).ToArray(), current.Tail, Loc);
            }
        }

        internal override bool Equal(SchemeObject otherObj, SchemeObjectPtrSet visited) {
            if (this == otherObj)
                return true;

            if (visited.Contains(this))
                return false;
            visited.Add(this);

            if (!(otherObj is SchemeList))
                return false;

            var other = (SchemeList)otherObj;
            return Head.Equal(other.Head, visited) && Tail.Equal(other.Head, visited);
        }

        public delegate void ForEachProc(params SchemeObject[] objects);

        public static void ForEach(
                ForEachProc proc, SchemeObject firstList, params SchemeObject[] otherLists) {
            SchemeObject[] lists = (new SchemeObject[] { firstList }).Concat(otherLists).ToArray();

            SchemeObjectPtrSet[] visiteds = new SchemeObjectPtrSet[lists.Length];
            for (int i = 0; i < visiteds.Length; i++)
                visiteds[i] = new SchemeObjectPtrSet();

            SchemeObject[] elements = new SchemeObject[lists.Length];

            while (true) {
                for (int i = 0; i < elements.Length; i++) {
                    SchemeObject list = lists[i];
                    if (!(list is SchemeList) || visiteds[i].Contains(list))
                        return;
                    visiteds[i].Add(list);
                    elements[i] = ((SchemeList)list).Head;
                    lists[i] = ((SchemeList)list).Tail;
                }

                proc(elements);
            }
        }

        public delegate SchemeObject MapProc(params SchemeObject[] objects);

        [SchemeBridge]
        public static SchemeObject Map(
                MapProc proc, SchemeObject firstList, params SchemeObject[] otherLists) {
            SchemeObject head = SchemeNull.Null, tail = SchemeNull.Null;
            ForEach((SchemeObject[] elements) => {
                SchemeObject mapped = proc(elements);

                SchemeList newTail = new SchemeList(mapped, SchemeNull.Null);
                if (head == SchemeNull.Null) {
                    head = tail = newTail;
                } else {
                    ((SchemeList)tail).Tail = newTail;
                    tail = newTail;
                }
            }, firstList, otherLists);

            return head;
        }

        [SchemeBridge("Length")]
        public static int ListLength(SchemeObject list) {
            int sum = 0;
            while (list is SchemeList) {
                list = ((SchemeList)list).Tail;
                sum++;
            }
            return sum;
        }

        [SchemeBridge]
        public SchemeObject Caar {
            get { return ((SchemeList)Head).Head; }
        }

        [SchemeBridge]
        public SchemeObject Cadr {
            get { return ((SchemeList)Head).Tail; }
        }

        [SchemeBridge]
        public SchemeObject Cdar {
            get { return ((SchemeList)Tail).Head; }
        }

        [SchemeBridge]
        public SchemeObject Cddr {
            get { return ((SchemeList)Tail).Tail; }
        }

        [SchemeBridge]
        public static SchemeObject Append(params SchemeObject[] lists) {
            SchemeObject newHead = SchemeNull.Null;
            SchemeList newTail = null;
            foreach (SchemeObject list in lists) {
                if (list is SchemeNull)
                    continue;
                if (!(list is SchemeList)) {
                    if (newTail == null)
                        newHead = list;
                    else
                        newTail.Tail = list;
                    return newHead;
                }

                foreach (SchemeObject obj in (SchemeList)list) {
                    SchemeList newPair = new SchemeList(obj, SchemeNull.Null);
                    if (newTail == null)
                        newHead = newPair;
                    else
                        newTail.Tail = newPair;
                    newTail = newPair;
                }
            }

            return newHead;
        }

        [SchemeBridge]
        public static SchemeObject Reverse(SchemeObject list) {
            SchemeObject prev = list, cur = SchemeNull.Null, next = SchemeNull.Null;
            if (prev is SchemeList) {
                cur = ((SchemeList)prev).Tail;
                ((SchemeList)prev).Tail = SchemeNull.Null;
            }
            if (cur is SchemeList)
                next = ((SchemeList)cur).Tail;

            while (cur is SchemeList) {
                ((SchemeList)cur).Tail = prev;

                prev = cur;
                cur = next;

                if (next is SchemeList)
                    next = ((SchemeList)next).Tail;
                else
                    next = SchemeNull.Null;
            }

            return prev;
        }

        [SchemeBridge]
        public static SchemeObject ListTail(SchemeObject list, int k) {
            while (k > 0) {
                list = ((SchemeList)list).Tail;
                k--;
            }
            return list;
        }

        [SchemeBridge]
        public static SchemeObject ListRef(SchemeObject list, int k) {
            return ((SchemeList)ListTail(list, k)).Head;
        }

        [SchemeBridge("ListSet!")]
        public static void ListSet(SchemeObject list, int k, SchemeObject obj) {
            while (k > 0) {
                ((SchemeList)list).Head = obj;
                list = ((SchemeList)list).Head;
                k--;
            }
        }

        [SchemeBridge]
        public static SchemeObject Member(
                SchemeObject obj,
                SchemeObject list,
                Func<SchemeObject, SchemeObject, bool> compare) {
            while (list is SchemeList) {
                if (compare(obj, ((SchemeList)list).Head))
                    return list;
                list = ((SchemeList)list).Tail;
            }

            return SchemeBool.False;
        }

        [SchemeBridge]
        public static SchemeObject Memq(SchemeObject obj, SchemeObject list) {
            return Member(obj, list, (a, b) => a.Eqv(b));
        }

        [SchemeBridge]
        public static SchemeObject Memv(SchemeObject obj, SchemeObject list) {
            return Member(obj, list, (a, b) => a.Eqv(b));
        }

        [SchemeBridge]
        public static SchemeObject Member(SchemeObject obj, SchemeObject list) {
            return Member(obj, list, (a, b) => a.Equal(b));
        }

        [SchemeBridge]
        public static SchemeObject Assoc(
                SchemeObject obj,
                SchemeObject aList,
                Func<SchemeObject, SchemeObject, bool> compare) {
            while (aList is SchemeList) {
                var current = (SchemeList)aList;
                if (compare(obj, ((SchemeList)current.Head).Head))
                    return current.Head;
                aList = ((SchemeList)aList).Tail;
            }

            return SchemeBool.False;
        }

        [SchemeBridge]
        public static SchemeObject Assq(SchemeObject obj, SchemeObject aList) {
            return Assoc(obj, aList, (a, b) => a.Eqv(b));
        }

        [SchemeBridge]
        public static SchemeObject Assv(SchemeObject obj, SchemeObject aList) {
            return Assoc(obj, aList, (a, b) => a.Eqv(b));
        }

        [SchemeBridge]
        public static SchemeObject Assoc(SchemeObject obj, SchemeObject aList) {
            return Assoc(obj, aList, (a, b) => a.Equal(b));
        }

        [SchemeBridge("List->Vector")]
        public static Vector ListToVector(SchemeList list) {
            return new Vector(list.ToArray());
        }

        [SchemeBridge("List->String")]
        public static string ListToString(SchemeList list) {
            return new String(list.Select(element => element.BridgeTo<char>()).ToArray());
        }

        internal override void FindCycles(SchemeObjectPtrSet visited, SchemeObjectPtrSet cycles) {
            if (MarkVisited(visited, cycles)) {
                Head.FindCycles(visited, cycles);
                Tail.FindCycles(visited, cycles);
            }
        }

        [SchemeBridge("List?")]
        public static bool IsList(SchemeObject arg) {
            SchemeObjectPtrSet visited = new SchemeObjectPtrSet();
            while (true) {
                // Check for cycles.
                if (!visited.Add(arg))
                    return false;

                if (arg is SchemeNull)
                    return true;
                if (!(arg is SchemeList))
                    return false;
                arg = ((SchemeList)arg).Tail;
            }

            throw new Exception("Unreachable");
        }

        [SchemeBridge]
        public static SchemeObject ListCopy(SchemeObject current) {
            SchemeObject first = null;
            SchemeList prev = null;
            SchemeObjectPtrSet visited = new SchemeObjectPtrSet();

            while (true) {
                // Check for cycles.
                if (!visited.Add(current))
                    throw new SchemeException("List is circular");

                if (!(current is SchemeList))
                    break;

                SchemeList copy = new SchemeList(((SchemeList)current).Head, SchemeNull.Null);
                if (first == null)
                    first = copy;
                else
                    prev.Tail = copy;

                prev = copy;
                current = ((SchemeList)current).Tail;
            }

            if (first == null)
                first = current;
            else
                prev.Tail = current;

            return first;
        }
    }

    internal class ListEnumerator : IEnumerator<SchemeObject> {
        private SchemeList mList;
        private SchemeList mCurrent;
        private bool mAtEnd;

        public ListEnumerator(SchemeList list) {
            mList = list;
            mCurrent = null;
            mAtEnd = false;
        }

        object IEnumerator.Current {
            get {
                return ((IEnumerator<SchemeObject>)this).Current;
            }
        }

        SchemeObject IEnumerator<SchemeObject>.Current {
            get {
                return mCurrent == null ? null : mCurrent.Head;
            }
        }

        public bool MoveNext() {
            if (mAtEnd)
                return false;

            // First element.
            if (mCurrent == null) {
                mCurrent = mList;
                return true;
            }

            // Rest of elements.
            if (mCurrent.Tail is SchemeList) {
                mCurrent = (SchemeList)mCurrent.Tail;
                return true;
            }

            mCurrent = null;
            mAtEnd = true;
            return false;
        }

        public void Reset() {
            mCurrent = null;
            mAtEnd = false;
        }

        public void Dispose() { }
    }
}
