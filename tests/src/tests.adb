with Unicfg; use Unicfg;
with Unicfg.YAML;
with Unicfg.JSON;
with Unicfg.TOML;
with Unicfg.XML;

with Ada.Text_IO;

procedure Tests is

   procedure Put (F : in out Ada.Text_IO.File_Type; Str : String) is
   begin
      Ada.Text_IO.Put (F, Str);
   end Put;

   procedure YAML_TIO_Dump
   is new Unicfg.YAML.Generic_Dump
     (Output_Stream => Ada.Text_IO.File_Type,
      Put => Put);

   procedure JSON_TIO_Dump
   is new Unicfg.JSON.Generic_Dump
     (Output_Stream => Ada.Text_IO.File_Type,
      Put => Put);

   procedure TOML_TIO_Dump
   is new Unicfg.TOML.Generic_Dump
     (Output_Stream => Ada.Text_IO.File_Type,
      Put => Put);

   procedure XML_TIO_Dump
   is new Unicfg.XML.Generic_Dump
     (Output_Stream => Ada.Text_IO.File_Type,
      Put => Put);

   Test : Node;
   Table, Table2, Table3, Table4 : Node;
   Vect_Of_Maps : Node;

   Vect : Node;
   Vect_Of_Vects : Node;
   File : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;

   type Plop_Arr is array (1 .. 10) of Integer;
   type Plop_Acc is access all Plop_Arr;

   Test_Arr : aliased Plop_Arr := (others => 42);
   Test_Acc : constant Plop_Acc := Test_Arr'Unchecked_Access;
begin

   Ada.Text_IO.Put_Line (Test_Acc.all (4)'Img);
   Test := Create_Vector;
   Table := Create_Table;
   for X in 1 .. 10 loop
      Test.Append (Create_String ("plop" & X'Img));
      Table.Set ("a" & X'Img, Create_String ("b" & X'Img));
   end loop;

   Table.Set ("rec", Test.Clone);
   Test.Append (Table);

   Table2 := Create_Table;
   Table2.Set ("mouarf", Create_String ("ok"));
   Table2.Set ("arr", Test.Clone);

   Table3 := Create_Table;
   Table3.Set ("table3_key1", Create_String ("ok"));
   Table3.Set ("table3_key2", Create_String ("FAIL"));
   Table3.Set ("xml", Create_String ("<TEST>"));
   Table3.Set ("123", Create_String ("& test &"));
   Table2.Set ("T3", Table3);

   Table4 := Create_Table;
   Table4.Set ("table4 key1", Create_String ("ok"));
   Table4.Set ("table4 ""key2""", Create_String ("FAIL"));
   Table4.Set ("table4.key3", Create_String ("FAIL"));
   Table3.Set ("T4", Table4);

   Vect_Of_Maps := Create_Vector;
   Vect_Of_Maps.Append (Table4.Clone);
   Vect_Of_Maps.Append (Table4.Clone);
   Table2.Set ("vect_of_maps", Vect_Of_Maps);

   Vect := Create_Vector;
   Vect.Append (Create_String ("a"));
   Vect.Append (Create_Int (42));
   Vect.Append (Create_Real (42.0));

   Vect_Of_Vects := Create_Vector;
   Vect_Of_Vects.Append (Vect.Clone);
   Vect_Of_Vects.Append (Vect.Clone);
   Vect_Of_Vects.Append (Vect.Clone);

   Table2.Set ("vect_of_vects", Vect_Of_Vects);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("# TOML");
   TOML_TIO_Dump (Table2, File);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("# JSON");
   JSON_TIO_Dump (Table2, File, 3);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("# JSON inline");
   JSON_TIO_Dump (Table2, File, 0);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("# YAML");
   YAML_TIO_Dump (Table2, File, 5);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("# YAML inline");
   YAML_TIO_Dump (Table2, File, 0);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("# XML");
   XML_TIO_Dump (Table2, File, 2);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("# XML inline");
   XML_TIO_Dump (Table2, File, 0);
end Tests;
