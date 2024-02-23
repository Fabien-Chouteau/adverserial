package body Adverserial.Table is

   ------------------
   -- Generic_Dump --
   ------------------

   procedure Generic_Dump  (This        :        Node;
                            Stream      : in out Output_Stream;
                            Headers     :        Colum_Header_Array)
   is
      use Ada.Strings.Unbounded;

      First : Boolean;
   begin
      First := True;
      for H of Headers loop
         if First then
            First := False;
         else
            Put (Stream, " | ");
         end if;
         Put (Stream, To_String (H.Display));
      end loop;
      Put (Stream, ASCII.LF & "");
      if This.Kind /= Vector then
         return;
      end if;

      for Index in 1 .. This.Length loop
         declare
            Elt : constant Node := This.Item (Index);
         begin
            if Elt.Kind = Map then
               First := True;
               for H of Headers loop
                  if First then
                     First := False;
                  else
                     Put (Stream, " | ");
                  end if;

                  if Elt.Has (H.Map_Key) then
                     Put (Stream, To_String (Elt.Get
                          (H.Map_Key).As_Unbounded_String));
                  end if;
               end loop;
               Put (Stream, ASCII.LF & "");
            end if;
         end;
      end loop;

   end Generic_Dump;

end Adverserial.Table;
