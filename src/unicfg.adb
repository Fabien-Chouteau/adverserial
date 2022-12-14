with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

with Interfaces;

package body Unicfg is

   use Ada.Strings.Unbounded;

   procedure Sort_Keys is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Positive,
      Element_Type => Unbounded_UTF8_String,
      Array_Type   => Key_Array);

   package TOML_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Node,
      Hash            => Hash,
      Equivalent_Keys => "=");

   package TOML_Vectors is new Ada.Containers.Vectors (Positive, Node);

   type Node_Record (Kind : Any_Node_Kind) is limited record
      Ref_Count : Natural;

      case Kind is
         when Map =>
            Map_Value : TOML_Maps.Map;

         when Vector =>
            Array_Value : TOML_Vectors.Vector;
            --  List of values for all items

            Array_Implicitly_Created : Boolean;
            --  Same as Table_Implicitly_Created

         when Value =>
            String_Value : Unbounded_String;

      end case;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Node_Record, Node_Record_Access);

   function Create_Node (Rec : Node_Record_Access) return Node;
   --  Wrap a value record in a value. This resets its ref-count to 1.

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node (Rec : Node_Record_Access) return Node is
   begin
      return Result : Node do
         Rec.Ref_Count := 1;
         Result.Value := Rec;
      end return;
   end Create_Node;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (This : Node) return Boolean is
   begin
      return This.Value = null;
   end Is_Null;

   ----------
   -- Kind --
   ----------

   function Kind (This : Node) return Any_Node_Kind is
   begin
      return This.Value.Kind;
   end Kind;

   ------------
   -- Equals --
   ------------

   function Equals (Left, Right : Node) return Boolean is
   begin
      --  If Left and Right refer to the same document, they are obviously
      --  equivalent (X is equivalent to X). If they don't have the same kind,
      --  they are obviously not equivalent.

      if Left = Right then
         return True;
      elsif Left.Kind /= Right.Kind then
         return False;
      end if;

      case Left.Kind is
         when Map =>
            declare
               Left_Keys  : constant Key_Array := Left.Keys;
               Right_Keys : constant Key_Array := Right.Keys;
            begin
               if Left_Keys /= Right_Keys then
                  return False;
               end if;

               for K of Left_Keys loop
                  if not Equals (Left.Get (K), Right.Get (K)) then
                     return False;
                  end if;
               end loop;
            end;

         when Vector =>
            if Left.Length /= Right.Length then
               return False;
            end if;

            for I in 1 .. Left.Length loop
               if not Equals (Left.Item (I), Right.Item (I)) then
                  return False;
               end if;
            end loop;

         when Value =>
            return Left.Value.String_Value = Right.Value.String_Value;

      end case;

      return True;
   end Equals;

   -----------
   -- Clone --
   -----------

   function Clone (This : Node) return Node is
      Result : Node;
   begin
      case This.Kind is
         when Map =>
            Result := Create_Table;
            for Key of This.Keys loop
               Result.Set (Key, This.Get (Key).Clone);
            end loop;

         when Vector =>
            Result := Create_Vector;
            for I in 1 .. This.Length loop
               Result.Append (This.Item (I));
            end loop;

         when Value =>
            Result := Create_String (This.Value.String_Value);
      end case;

      return Result;
   end Clone;

   ---------------
   -- As_String --
   ---------------

   function As_String (This : Node) return String is
   begin
      return To_String (This.As_Unbounded_String);
   end As_String;

   -------------------------
   -- As_Unbounded_String --
   -------------------------

   function As_Unbounded_String
     (This : Node) return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return This.Value.String_Value;
   end As_Unbounded_String;

   ---------
   -- Has --
   ---------

   function Has (This : Node; Key : String) return Boolean is
   begin
      return This.Has (To_Unbounded_String (Key));
   end Has;

   ---------
   -- Has --
   ---------

   function Has
     (This : Node; Key : Unbounded_UTF8_String) return Boolean is
   begin
      return This.Value.Map_Value.Contains (Key);
   end Has;

   ----------
   -- Keys --
   ----------

   function Keys (This : Node) return Key_Array is
      use TOML_Maps;
      Map : TOML_Maps.Map renames This.Value.Map_Value;
      I   : Positive := 1;
   begin
      return Result : Key_Array (1 .. Natural (Map.Length)) do
         for Position in Map.Iterate loop
            Result (I) := Key (Position);
            I := I + 1;
         end loop;
         Sort_Keys (Result);
      end return;
   end Keys;

   ---------
   -- Get --
   ---------

   function Get (This : Node; Key : String) return Node is
   begin
      return This.Get (To_Unbounded_String (Key));
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (This : Node; Key : Unbounded_UTF8_String) return Node is
   begin
      return This.Value.Map_Value.Element (Key);
   end Get;

   -----------------
   -- Get_Or_Null --
   -----------------

   function Get_Or_Null (This : Node; Key : String) return Node
   is
   begin
      return This.Get_Or_Null (To_Unbounded_String (Key));
   end Get_Or_Null;

   -----------------
   -- Get_Or_Null --
   -----------------

   function Get_Or_Null
     (This : Node; Key : Unbounded_UTF8_String) return Node
   is
      use TOML_Maps;
      Position : constant Cursor := This.Value.Map_Value.Find (Key);
   begin
      return (if Has_Element (Position)
              then Element (Position)
              else No_Value);
   end Get_Or_Null;

   ----------------------
   -- Iterate_On_Table --
   ----------------------

   function Iterate_On_Table (This : Node) return Table_Entry_Array is
      Keys : constant Key_Array := This.Keys;
   begin
      return Result : Table_Entry_Array (Keys'Range) do
         for I in Keys'Range loop
            Result (I) := (Keys (I), This.Get (Keys (I)));
         end loop;
      end return;
   end Iterate_On_Table;

   ------------
   -- Length --
   ------------

   function Length (This : Node) return Natural is
   begin
      return Natural (This.Value.Array_Value.Length);
   end Length;

   ----------
   -- Item --
   ----------

   function Item (This : Node; Index : Positive) return Node is
   begin
      return This.Value.Array_Value.Element (Index);
   end Item;

   -------------------
   -- Create_String --
   -------------------

   function Create_String (Str : String) return Node is
   begin
      return Create_String (To_Unbounded_String (Str));
   end Create_String;

   -------------------
   -- Create_String --
   -------------------

   function Create_String (Str : Unbounded_UTF8_String) return Node is
   begin
      return Create_Node (new Node_Record'
                            (Kind         => Value,
                             Ref_Count    => 1,
                             String_Value => Str));
   end Create_String;

   ------------------
   -- Create_Table --
   ------------------

   function Create_Table return Node is
   begin
      return Create_Node (new Node_Record'
                            (Kind                     => Map,
                             Ref_Count                => 1,
                             Map_Value                => <>));
   end Create_Table;

   ---------
   -- Set --
   ---------

   procedure Set (This : Node; Key : String; Ent : Node)
   is
   begin
      This.Set (To_Unbounded_String (Key), Ent);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (This  : Node;
      Key   : Unbounded_UTF8_String;
      Ent   : Node)
   is
   begin
      This.Value.Map_Value.Include (Key, Ent);
   end Set;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default
     (This : Node; Key : String; Ent : Node)
   is
   begin
      This.Set_Default (To_Unbounded_String (Key), Ent);
   end Set_Default;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default
     (This : Node;
      Key  : Unbounded_UTF8_String;
      Ent  : Node)
   is
      use TOML_Maps;
      Dummy_Position : Cursor;
      Dummy_Inserted : Boolean;
   begin
      This.Value.Map_Value.Insert
        (Key, Ent, Dummy_Position, Dummy_Inserted);
   end Set_Default;

   -----------
   -- Unset --
   -----------

   procedure Unset (This : Node; Key : String) is
   begin
      This.Unset (To_Unbounded_String (Key));
   end Unset;

   -----------
   -- Unset --
   -----------

   procedure Unset (This : Node; Key : Unbounded_UTF8_String) is
   begin
      This.Value.Map_Value.Delete (Key);
   end Unset;

   -----------
   -- Merge --
   -----------

   function Merge (L, R : Node) return Node is
      function Merge_Entries
        (Key              : Unbounded_UTF8_String;
         Dummy_L, Dummy_R : Node) return Node
      is (raise Constraint_Error with "duplicate key: " & To_String (Key));
   begin
      return Merge (L, R, Merge_Entries'Access);
   end Merge;

   -----------
   -- Merge --
   -----------

   function Merge
     (L, R          : Node;
      Merge_Entries : not null access function
        (Key : Unbounded_UTF8_String; L, R : Node) return Node)
      return Node
   is
      Table : constant Node := Create_Table;
   begin
      for Key of L.Keys loop
         Table.Set (Key, L.Get (Key));
      end loop;

      for Key of R.Keys loop
         declare
            N : Node := R.Get (Key);
         begin
            if Table.Has (Key) then
               N := Merge_Entries (Key, Table.Get (Key), N);
            end if;
            Table.Set (Key, N);
         end;
      end loop;

      return Table;
   end Merge;

   -------------------
   -- Create_Vector --
   -------------------

   function Create_Vector return Node is
   begin
      return Create_Node (new Node_Record'
                            (Kind                     => Vector,
                             Ref_Count                => 1,
                             Array_Value              => <>,
                             Array_Implicitly_Created => False));
   end Create_Vector;

   ---------
   -- Set --
   ---------

   procedure Set (This : Node; Index : Positive; Item : Node) is
   begin
      This.Value.Array_Value (Index) := Item;
   end Set;

   ------------
   -- Append --
   ------------

   procedure Append (Value, Item : Node) is
   begin
      Value.Value.Array_Value.Append (Item);
   end Append;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before
     (This : Node; Index : Positive; Item : Node)
   is
   begin
      This.Value.Array_Value.Insert (Index, Item);
   end Insert_Before;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (This : in out Node) is
   begin
      if This.Value = null then
         return;
      end if;

      This.Value.Ref_Count := This.Value.Ref_Count + 1;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (This : in out Node) is
   begin
      if This.Value = null then
         return;
      end if;

      declare
         V : Node_Record renames This.Value.all;
      begin
         --  Decrement the ref-count. If no-one references V anymore,
         --  deallocate it.

         V.Ref_Count := V.Ref_Count - 1;
         if V.Ref_Count > 0 then
            return;
         end if;
      end;

      Free (This.Value);
   end Finalize;

   -------------------
   -- Format_String --
   -------------------

   function Format_String (S : Unbounded_UTF8_String)
                           return Unbounded_UTF8_String
   is
      Result : Unbounded_UTF8_String;
   begin
      Append (Result, """");
      for I in 1 .. Length (S) loop
         --  Forward printable ASCII bytes (except quotes and backslashes)
         --  and non-ASCII bytes, but emit escapes for quotes and control
         --  characters.
         declare
            Char : constant Character := Element (S, I);
         begin
            case Char is
               --  The only way to represent the following control characters
               --  is to use the unicode escape (\uXXXX).
               when ASCII.NUL .. ASCII.BEL
                  | ASCII.VT
                  | ASCII.SO .. ASCII.US
                  | ASCII.DEL
               =>
                  declare
                     use type Interfaces.Unsigned_8;

                     Byte : Interfaces.Unsigned_8 := Character'Pos (Char);
                     Repr : String (1 .. 6) := "\u0000";
                     I    : Natural range 3 .. 6 := 6;
                  begin
                     while Byte /= 0 loop
                        declare
                           Nibble : constant Interfaces.Unsigned_8 :=
                              Byte mod 16;
                           Digit  : Interfaces.Unsigned_8;
                        begin
                           if Nibble <= 9 then
                              Digit := Character'Pos ('0') + Nibble;
                           else
                              Digit := Character'Pos ('a') - 10 + Nibble;
                           end if;
                           Repr (I) := Character'Val (Digit);
                           Byte := Byte / 16;
                           I := I - 1;
                        end;
                     end loop;
                     Append (Result, Repr);
                  end;

               --  The following control characters have dedicated escape
               --  sequences:

               when ASCII.BS => Append (Result, "\b");
               when ASCII.HT => Append (Result, "\t");
               when ASCII.LF => Append (Result, "\n");
               when ASCII.FF => Append (Result, "\f");
               when ASCII.CR => Append (Result, "\r");

               --  Quotes and backslashes must be escaped

               when '"'      => Append (Result, "\""");
               when '\'      => Append (Result, "\\");

               --  All other bytes can be directly forwarded to the string
               --  literal.

               when ' ' | '!'
                  | '#' .. '['
                  | ']' .. '~'
                  | Character'Val (128) .. Character'Val (255)
               =>
                  Append (Result, Char);
            end case;
         end;
      end loop;
      Append (Result, """");
      return Result;
   end Format_String;

   -------------------
   -- Indent_String --
   -------------------

   function Indent_String (Count : Natural) return Unbounded_UTF8_String is
   begin
      return To_Unbounded_String (String'(1 .. Count => ' '));
   end Indent_String;

end Unicfg;
