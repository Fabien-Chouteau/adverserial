
package body Unicfg.JSON is

   ------------------
   -- Generic_Dump --
   ------------------

   procedure Generic_Dump  (This        : Node;
                            Stream      : in out Output_Stream;
                            Indent_Step : Natural := 1)
   is
      use Ada.Strings.Unbounded;

      Item_Separator : constant String := ", ";
      Key_Separator  : constant String := ": ";

      Indent_Level : Natural := 0;
      New_Line_Indent : Unbounded_UTF8_String;

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

      procedure Dump_Rec (N : Node) is
         First : Boolean := True;
         Separator : Unbounded_UTF8_String;

         -----------------
         -- Indent_Down --
         -----------------

         procedure Indent_Down is
         begin
            if Indent_Step /= 0 then
               Indent_Level := Indent_Level + Indent_Step;
               New_Line_Indent := ASCII.LF & Indent_String (Indent_Level);
               Separator := Item_Separator & New_Line_Indent;

               Put (New_Line_Indent);
            else
               New_Line_Indent := To_Unbounded_String ("");
               Separator := To_Unbounded_String (Item_Separator);
            end if;
         end Indent_Down;
         ---------------
         -- Indent_Up --
         ---------------

         procedure Indent_Up is
         begin
            if Indent_Level /= 0 then
               Indent_Level := Indent_Level - Indent_Step;
               Put (ASCII.LF & Indent_String (Indent_Level));
            end if;
         end Indent_Up;

      begin
         case N.Kind is
            when Map =>
               Put ("{");
               Indent_Down;

               for Ent of N.Iterate_On_Table loop
                  if First then
                     First := False;
                  else
                     Put (Separator);
                  end if;

                  Put (Format_String (Ent.Key));
                  Put (Stream, Key_Separator);

                  Dump_Rec (Ent.Value);
               end loop;

               Indent_Up;
               Put ("}");

            when Vector =>
               Put ("[");
               Indent_Down;

               for Index in 1 .. N.Length loop
                  if First then
                     First := False;
                  else
                     Put (Separator);
                  end if;

                  Dump_Rec (N.Item (Index));
               end loop;

               Indent_Up;
               Put ("]");

            when Value =>
               Put (Format_String (N.As_Unbounded_String));

         end case;
      end Dump_Rec;

   begin
      Dump_Rec (This);
   end Generic_Dump;

end Unicfg.JSON;
