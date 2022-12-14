
package body Unicfg.TOML is

   ----------------
   -- Format_Key --
   ----------------

   function Format_Key (Key : Unbounded_UTF8_String)
                        return Unbounded_UTF8_String
   is
      use Ada.Strings.Unbounded;
   begin
      --  Determine if we need to quote Key, and if so, do it

      for I in 1 .. Length (Key) loop
         if Element (Key, I) not in
            '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '-' | '_'
         then
            return Format_String (Key);
         end if;
      end loop;

      if Length (Key) = 0 then
         return Format_String (Key);
      end if;

      --  Otherwise, we can return the key as-is (without quoting)

      return Key;
   end Format_Key;

   ------------------
   -- Generic_Dump --
   ------------------

   procedure Generic_Dump  (This        : Node;
                            Stream      : in out Output_Stream)
   is
      use Ada.Strings.Unbounded;

      Item_Separator : constant String := ", ";
      Key_Separator : constant String := " = ";

      Map_Key_Acc : Unbounded_UTF8_String;

      ---------
      -- Put --
      ---------

      procedure Put (Str : Ada.Strings.Unbounded.Unbounded_String) is
      begin
         Put (Stream, Ada.Strings.Unbounded.To_String (Str));
      end Put;

      ---------
      -- Put --
      ---------

      procedure Put (Str : String) is
      begin
         Put (Stream, Str);
      end Put;

      --------------
      -- Dump_Rec --
      --------------

      procedure Dump_Rec (N : Node; Table_Inline : Boolean := False) is
         First : Boolean := True;
      begin
         case N.Kind is
            when Map =>
               if Table_Inline then
                  Put ("{");
                  for Ent of N.Iterate_On_Table loop
                     if First then
                        First := False;
                     else
                        Put (Item_Separator);
                     end if;

                     Put ("""");
                     Put (To_String (Ent.Key));
                     Put ("""");
                     Put (Key_Separator);

                     Dump_Rec (Ent.Value, Table_Inline);
                  end loop;
                  Put ("}");

               else
                  --  First, output only the values that are not tables/maps
                  for Ent of N.Iterate_On_Table loop
                     if Ent.Value.Kind /= Map then

                        Put (Format_Key (Ent.Key));
                        Put (Key_Separator);

                        Dump_Rec (Ent.Value, Table_Inline);
                        Put ("" & ASCII.LF);
                     end if;
                  end loop;

                  --  Second, output the values that are tables/maps
                  for Ent of N.Iterate_On_Table loop
                     if Ent.Value.Kind = Map then
                        if Length (Map_Key_Acc) /= 0 then
                           Append (Map_Key_Acc, ".");
                        end if;
                        Append (Map_Key_Acc, Format_Key (Ent.Key));

                        Put ("[");
                        Put (Map_Key_Acc);
                        Put ("]" & ASCII.LF);

                        Dump_Rec (Ent.Value, Table_Inline);
                     end if;
                  end loop;
               end if;

            when Vector =>
               Put ("[");
               for Index in 1 .. N.Length loop
                  if First then
                     First := False;
                  else
                     Put (Item_Separator);
                  end if;
                  Dump_Rec (N.Item (Index),

                            --  Tables in arrays have to be inlined
                            Table_Inline => True);
               end loop;

               Put ("]");

            when Value =>
               Put (Format_String (N.As_Unbounded_String));
         end case;
      end Dump_Rec;

   begin
      Dump_Rec (This);
   end Generic_Dump;

end Unicfg.TOML;
