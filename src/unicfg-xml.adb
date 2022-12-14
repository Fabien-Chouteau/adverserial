
package body Unicfg.XML is

   ------------------
   -- Element_Name --
   ------------------

   function Element_Name (Key : Unbounded_UTF8_String)
                          return Unbounded_UTF8_String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_UTF8_String;
   begin
      --  Must start with a letter or underscore
      if Length (Key) >= 1
        and then
         Element (Key, 1) not in 'a' .. 'z' | 'A' .. 'Z' | '_'
      then
         Append (Result, "_");
      end if;

      --  Cannot start with the letters xml
      if Length (Key) >= 3
        and then Element (Key, 1) in 'x' | 'X'
        and then Element (Key, 2) in 'm' | 'M'
        and then Element (Key, 3) in 'l' | 'L'
      then
         Append (Result, "_");
      end if;

      for I in 1 .. Length (Key) loop
         declare
            C : constant Character := Element (Key, I);
         begin
            if C in '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-' | '_' | '.'
            then
               Append (Result, C);
            else
               Append (Result, "_");
            end if;
         end;
      end loop;
      return Result;
   end Element_Name;

   ------------------
   -- Element_Name --
   ------------------

   function Escape_Data (Data : Unbounded_UTF8_String)
                         return Unbounded_UTF8_String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_UTF8_String;
   begin
      for I in 1 .. Length (Data) loop
         declare
            C : constant Character := Element (Data, I);
         begin
            case C is
               when '"' => Append (Result, "&quot;");
               when ''' => Append (Result, "&apos;");
               when '<' => Append (Result, "&lt;");
               when '>' => Append (Result, "&gt;");
               when '&' => Append (Result, "&amp;");
               when others =>
                  Append (Result, C);
            end case;
         end;
      end loop;
      return Result;
   end Escape_Data;

   ------------------
   -- Generic_Dump --
   ------------------

   procedure Generic_Dump  (This        :        Node;
                            Stream      : in out Output_Stream;
                            Indent_Step :        Natural := 1)
   is
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
         --  Put ("#down (" & Indent_Level'Img & ")#");
      end Down;

      --------
      -- Up --
      --------

      procedure Up is
      begin
         Indent_Level := Indent_Level - Indent_Step;
         Indent := Indent_String (Indent_Level);
         --  Put ("#up (" & Indent_Level'Img & ")#");
      end Up;

      --------------
      -- End_Line --
      --------------

      procedure End_Line is
      begin
         if Indent_Step /= 0 then
            Put ("" & ASCII.LF);
         end if;
      end End_Line;

      --------------
      -- Dump_Rec --
      --------------

      procedure Dump_Rec (N : Node) is
      begin
         case N.Kind is
            when Map =>
               for Ent of N.Iterate_On_Table loop
                  declare
                     Name : constant Unbounded_UTF8_String :=
                       Element_Name (Ent.Key);
                  begin
                     Put (Indent);
                     Put ("<");
                     Put (Name);
                     Put (">");

                     if Ent.Value.Kind in Composite_Node_Kind then
                        End_Line;
                        Down;
                        Dump_Rec (Ent.Value);
                        Up;
                        Put (Indent);
                     else
                        Dump_Rec (Ent.Value);
                     end if;
                     Put ("</");
                     Put (Name);
                     Put (">");
                     End_Line;
                  end;
               end loop;

            when Vector =>
               for Index in 1 .. N.Length loop
                  declare
                     Value : constant Node := N.Item (Index);
                  begin
                     Put (Indent);
                     Put ("<item>");

                     if Value.Kind in Composite_Node_Kind then
                        End_Line;
                        Down;
                        Dump_Rec (Value);
                        Up;
                        Put (Indent);
                     else
                        Dump_Rec (Value);
                     end if;
                     Put ("</item>");
                     End_Line;
                  end;
               end loop;

            when Value =>
               Put (Escape_Data (N.As_Unbounded_String));
         end case;
      end Dump_Rec;

   begin
      Dump_Rec (This);
   end Generic_Dump;

end Unicfg.XML;
