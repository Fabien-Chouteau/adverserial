with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals;

package Adverserial
with Preelaborate
is

   pragma Warnings (Off);
   use type Ada.Strings.Unbounded.Unbounded_String;
   use type Ada.Numerics.Big_Numbers.Big_Integers.Big_Integer;
   use type Ada.Numerics.Big_Numbers.Big_Reals.Big_Real;
   pragma Warnings (On);

   subtype Unbounded_UTF8_String is Ada.Strings.Unbounded.Unbounded_String;
   subtype Big_Integer is Ada.Numerics.Big_Numbers.Big_Integers.Big_Integer;
   subtype Big_Real is Ada.Numerics.Big_Numbers.Big_Reals.Big_Real;

   type Any_Node_Kind is
     (Map,
      Vector,
      String_Value,
      Int_Value,
      Real_Value);

   subtype Composite_Node_Kind is
      Any_Node_Kind range Map .. Vector;

   subtype Atom_Node_Kind is
      Any_Node_Kind range String_Value .. Real_Value;

   type Node is new Ada.Finalization.Controlled with private;
   No_Value : constant Node;

   -----------------------
   -- Generic accessors --
   -----------------------

   function Is_Null (This : Node) return Boolean;
   --  Return whether Value is a null reference

   function Is_Present (This : Node) return Boolean
   is (not This.Is_Null);

   function Kind (This : Node) return Any_Node_Kind
      with Pre => This.Is_Present;
   --  Return the kind of node

   function Equals (Left, Right : Node) return Boolean
      with Pre => Left.Is_Present and then Right.Is_Present;
   --  Return whether Left and Right refer to equivalent documents.
   --
   --  Note that this is very different from the built-in "=" operator:
   --  the Node type has by-reference meaning, so "=" compares identity,
   --  not structural equivalence.

   function Clone (This : Node) return Node
      with Pre => This.Is_Present;
   --  Return a reference to a deep copy for Value

   --------------------
   -- Atom accessors --
   --------------------

   function As_String (This : Node) return String
      with Pre => This.Kind = String_Value;
   --  Return the string that Value represents

   function As_Unbounded_String
     (This : Node) return Unbounded_UTF8_String
      with Pre => This.Kind = String_Value;
   --  Likewise, but return an unbounded string

   function As_Int
     (This : Node) return Big_Integer
      with Pre => This.Kind = Int_Value;
   --  Return the integer that Value represents

   function As_Real
     (This : Node) return Big_Real
      with Pre => This.Kind = Real_Value;
   --  Return the real that Value represents

   ---------------------
   -- Table accessors --
   ---------------------

   function Has (This : Node; Key : String) return Boolean
      with Pre => This.Kind = Map;
   --  Return whether Value contains an entry for the given Key

   function Has
     (This : Node; Key : Unbounded_UTF8_String) return Boolean
      with Pre => This.Kind = Map;
   --  Likewise, but take an unbounded string

   type Key_Array is array (Positive range <>) of Unbounded_UTF8_String;

   function Keys (This : Node) return Key_Array
      with Pre => This.Kind = Map;
   --  Return a list for all keys in the given table. Note that the result is
   --  sorted.

   function Get (This : Node; Key : String) return Node
      with Pre => This.Has (Key);
   --  Return the value for the entry in Value corresponding to Key

   function Get
     (This : Node; Key : Unbounded_UTF8_String) return Node
      with Pre => This.Has (Key);
   --  Likewise, but take an unbounded string

   function Get_Or_Null (This : Node; Key : String) return Node
      with Pre => This.Kind = Map;
   --  If there is an entry in the Value table, return its value. Return
   --  No_Node otherwise.

   function Get_Or_Null
     (This : Node; Key : Unbounded_UTF8_String) return Node
      with Pre => This.Kind = Map;
   --  Likewise, but take an unbounded string

   --  The following types and primitive allow one to iterate on key/value
   --  entries conveniently in a simple FOR loop.

   type Table_Entry is record
      Key   : Unbounded_UTF8_String;
      Value : Node;
   end record;

   type Table_Entry_Array is array (Positive range <>) of Table_Entry;

   function Iterate_On_Table (This : Node) return Table_Entry_Array
      with Pre => This.Kind = Map;
   --  Return an array of key/value pairs for all entries in Value. The result
   --  is sorted by key.

   ---------------------
   -- Array accessors --
   ---------------------

   function Length (This : Node) return Natural
      with Pre => This.Kind = Vector;
   --  Return the number of items in Value

   function Item (This : Node; Index : Positive) return Node
      with Pre  => This.Kind = Vector and then Index <= This.Length;
   --  Return the item in Value at the given Index

   -------------------
   -- Atom creators --
   -------------------

   function Create_String (Str : String) return Node
      with Post => Create_String'Result.Kind = String_Value
                   and then Create_String'Result.As_String = Str;
   --  Create a TOML string value. Value must be a valid UTF-8 string.

   function Create_String (Str : Unbounded_UTF8_String) return Node
      with Post => Create_String'Result.Kind = String_Value
                   and then Create_String'Result.As_Unbounded_String = Str;
   --  Create a TOML string value

   function Create_Int (V : Big_Integer) return Node
      with Post => Create_Int'Result.Kind = Int_Value
                   and then Create_Int'Result.As_Int = V;
   --  Create an integer value

   function Create_Real (V : Big_Real) return Node
      with Post => Create_Real'Result.Kind = Real_Value
                   and then Create_Real'Result.As_Real = V;
   --  Create an real value

   ---------------------
   -- Table modifiers --
   ---------------------

   function Create_Table return Node
      with Post => Create_Table'Result.Kind = Map;
   --  Create an empty TOML table

   procedure Set (This : Node; Key : String; Ent : Node)
      with Pre => This.Kind = Map;
   --  Create an entry in Value to bind Key to Entry_Value. If Value already
   --  has an entry for Key, replace it.

   procedure Set
     (This  : Node;
      Key   : Unbounded_UTF8_String;
      Ent   : Node)
      with Pre => This.Kind = Map;
   --  Likewise, but take an unbounded string

   procedure Set_Default
     (This : Node; Key : String; Ent : Node)
      with Pre => Ent.Kind = Map;
   --  If Value has an entry for Key, do nothing. Otherwise, create an entry
   --  binding Key to Entry_Value.

   procedure Set_Default
     (This : Node;
      Key  : Unbounded_UTF8_String;
      Ent  : Node)
      with Pre => This.Kind = Map;
   --  Likewise, but take an unbounded string

   procedure Unset (This : Node; Key : String)
      with Pre => This.Kind = Map and then This.Has (Key);
   --  Remove the Key entry in Value

   procedure Unset (This : Node; Key : Unbounded_UTF8_String)
      with Pre => This.Kind = Map and then This.Has (Key);
   --  Likewise, but take an unbounded string

   function Merge (L, R : Node) return Node
      with Pre  => L.Kind = Map and then R.Kind = Map,
           Post => Merge'Result.Kind = Map;
   --  Merge two tables. If a key is present in both, Constraint_Error is
   --  raised. The operation is shallow, so the result table shares values with
   --  L and R.

   function Merge
     (L, R          : Node;
      Merge_Entries : not null access function
        (Key : Unbounded_UTF8_String; L, R : Node) return Node)
      return Node
      with Pre  => L.Kind = Map and then R.Kind = Map,
           Post => Merge'Result.Kind = Map;
   --  Merge two tables. If a key is present in both, call Merge_Entries to
   --  resolve the conflict: use its return value for the entry in the returned
   --  table.

   ----------------------
   -- Vector modifiers --
   ----------------------

   function Create_Vector return Node
      with Post => Create_Vector'Result.Kind = Vector;
   --  Create a TOML array

   procedure Set (This : Node; Index : Positive; Item : Node)
      with Pre => This.Kind = Vector and then Index <= This.Length;
   --  Replace the Index'th item in Value with Item

   procedure Append (Value, Item : Node)
      with Pre => Value.Kind = Vector;
   --  Append Item to the Value array

   procedure Insert_Before
     (This : Node; Index : Positive; Item : Node)
      with Pre => This.Kind = Vector and then Index < This.Length + 1;
   --  Insert Item before the Item'th element in the Value array

private

   type Node_Record;
   type Node_Record_Access is access all Node_Record;

   type Node is new Ada.Finalization.Controlled with record
      Value : Node_Record_Access;
   end record;

   overriding procedure Adjust (This : in out Node);
   overriding procedure Finalize (This : in out Node);

   No_Value : constant Node := (Ada.Finalization.Controlled
                                  with Value => null);

   function Format_String (S : Unbounded_UTF8_String)
                           return Unbounded_UTF8_String;
   --  A string formatting function common to all currently available outputs

   function Indent_String (Count : Natural) return Unbounded_UTF8_String;

end Adverserial;
