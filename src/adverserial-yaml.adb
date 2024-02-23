package body Adverserial.YAML is

   ------------------
   -- Generic_Dump --
   ------------------

   procedure Generic_Dump  (This        :        Node;
                            Stream      : in out Output_Stream;
                            Indent_Step :        Natural := 2)
   is
      Item_Separator : constant String := ", ";
      Key_Separator  : constant String := ": ";

      Indent_Level : Natural := 0;
      Indent : Unbounded_UTF8_String;

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

      ----------
      -- Down --
      ----------

      procedure Down is
      begin
         Indent_Level := Indent_Level + Indent_Step;
         Indent := Indent_String (Indent_Level);
      end Down;

      --------
      -- Up --
      --------

      procedure Up is
      begin
         Indent_Level := Indent_Level - Indent_Step;
         Indent := Indent_String (Indent_Level);
      end Up;

      --------------
      -- End_Line --
      --------------

      procedure End_Line is
      begin
         Put ("" & ASCII.LF);
      end End_Line;

      --------------
      -- Dump_Rec --
      --------------

      procedure Dump_Rec (N : Node) is
      begin
         case N.Kind is
            when Map =>
               for Ent of N.Iterate_On_Table loop
                  Put (Indent);

                  Put (Format_String (Ent.Key));
                  Put (Key_Separator);

                  if Ent.Value.Kind in Composite_Node_Kind then
                     End_Line;
                     Down;
                     Dump_Rec (Ent.Value);
                     Up;
                  else
                     Dump_Rec (Ent.Value);
                     End_Line;
                  end if;
               end loop;

            when Vector =>
               for Index in 1 .. N.Length loop
                  declare
                     Value : constant Node := N.Item (Index);
                  begin
                     Put (Indent);
                     Put ("- ");

                     if Value.Kind in Composite_Node_Kind then
                        End_Line;
                        Down;
                        Dump_Rec (Value);
                        Up;
                     else
                        Dump_Rec (Value);
                        End_Line;
                     end if;
                  end;
               end loop;

            when String_Value =>
               Put (Format_String (N.As_Unbounded_String));

            when Int_Value =>
               Put
                 (Ada.Numerics.Big_Numbers.Big_Integers.To_String (N.As_Int));

            when Real_Value =>
               Put
                 (Ada.Numerics.Big_Numbers.Big_Reals.To_String (N.As_Real));

         end case;
      end Dump_Rec;

      ---------------------
      -- Dump_Inline_Rec --
      ---------------------

      procedure Dump_Inline_Rec (N : Node) is
         First : Boolean := True;
      begin
         case N.Kind is
            when Map =>
               Put ("{");
               for Ent of N.Iterate_On_Table loop
                  if First then
                     First := False;
                  else
                     Put (Item_Separator);
                  end if;

                  Put (Format_String (Ent.Key));
                  Put (Key_Separator);

                  Dump_Inline_Rec (Ent.Value);
               end loop;
               Put ("}");

            when Vector =>
               Put ("[");
               for Index in 1 .. N.Length loop
                  if First then
                     First := False;
                  else
                     Put (Item_Separator);
                  end if;

                  Dump_Inline_Rec (N.Item (Index));
               end loop;
               Put ("]");

            when String_Value =>
               Put (Format_String (N.As_Unbounded_String));

            when Int_Value =>
               Put
                 (Ada.Numerics.Big_Numbers.Big_Integers.To_String (N.As_Int));

            when Real_Value =>
               Put
                 (Ada.Numerics.Big_Numbers.Big_Reals.To_String (N.As_Real));

         end case;
      end Dump_Inline_Rec;

   begin
      if Indent_Step /= 0 then
         Dump_Rec (This);
      else
         Dump_Inline_Rec (This);
      end if;
   end Generic_Dump;

end Adverserial.YAML;
