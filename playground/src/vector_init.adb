pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Vector_Init is

   type Color is (Yellow, Orange, Red, Green, Blue, Violet);

   package Color_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Color
   );

   Colors : array (1 .. 10) of Color_Vectors.Vector;

begin

   Colors := [others => Color_Vectors.Empty_Vector];
   -- This is also what happens implicitly, if this line is not added.

   Colors (1).Append (Yellow);
   Colors (1).Append (Blue);
   Colors (2).Append (Green);

   Ada.Text_IO.Put_Line (Colors'Image);

end Vector_Init;