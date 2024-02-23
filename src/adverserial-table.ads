package Adverserial.Table
with Preelaborate
is

   type Colum_Header is record
      Display : Unbounded_UTF8_String;
      Map_Key : Unbounded_UTF8_String;
   end record;

   type Colum_Header_Array is array (Natural range <>) of Colum_Header;

   generic
      type Output_Stream (<>) is limited private;
      --  Stream of bytes

      with procedure Put (Stream : in out Output_Stream; Bytes : String) is <>;
      --  Write all Bytes in Stream

   procedure Generic_Dump  (This        :        Node;
                            Stream      : in out Output_Stream;
                            Headers     :        Colum_Header_Array);

end Adverserial.Table;
