package Adverserial.XML
with Preelaborate
is
   generic
      type Output_Stream (<>) is limited private;
      --  Stream of bytes

      with procedure Put (Stream : in out Output_Stream; Bytes : String) is <>;
      --  Write all Bytes in Stream

   procedure Generic_Dump  (This        :        Node;
                            Stream      : in out Output_Stream;
                            Indent_Step :        Natural := 1);

end Adverserial.XML;
