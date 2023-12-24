-- with Ada.Text_IO;
with Contraption; use Contraption;
with Contraption.Energize; use Contraption.Energize;

package body AOC2023_16 is

   -- Part 1
   function Number_Of_Energized_Tiles return Natural is
      Input_Grid      : Grid;
      Energized_Cells : Natural;
   begin
      Input_Grid      := Parse_Input (Input_File_Name);
      Energized_Cells := Send_Beam_Through (Input_Grid,
         Row => 1,
         Col => 1,
         Dir => Right
      );

      return Energized_Cells;
   end Number_Of_Energized_Tiles;

   -- Part 2
   function Max_Number_Of_Energized_Tiles return Natural is
      Input_Grid : Grid;
      Max_Energy : Natural := 0;
   begin
      Input_Grid := Parse_Input (Input_File_Name);

      for C in Columns'Range loop
         Max_Energy := Natural'Max (@, Send_Beam_Through (Input_Grid, Rows'First, C, Down));
         Max_Energy := Natural'Max (@, Send_Beam_Through (Input_Grid, Rows'Last, C, Up));

         if C > Columns'First then
            Max_Energy := Natural'Max (@, Send_Beam_Through (Input_Grid, Rows'First, C, Left));
            Max_Energy := Natural'Max (@, Send_Beam_Through (Input_Grid, Rows'Last, C, Left));
         end if;

         if C < Columns'Last then
            Max_Energy := Natural'Max (@, Send_Beam_Through (Input_Grid, Rows'First, C, Right));
            Max_Energy := Natural'Max (@, Send_Beam_Through (Input_Grid, Rows'Last, C, Right));
         end if;
      end loop;

      for R in Rows'Range loop
         Max_Energy := Natural'Max (@, Send_Beam_Through (Input_Grid, R, Columns'First, Right));
         Max_Energy := Natural'Max (@, Send_Beam_Through (Input_Grid, R, Columns'Last, Left));

         if R > Rows'First then
            Max_Energy := Natural'Max (@, Send_Beam_Through (Input_Grid, R, Columns'First, Up));
            Max_Energy := Natural'Max (@, Send_Beam_Through (Input_Grid, R, Columns'Last, Up));
         end if;

         if R < Rows'Last then
            Max_Energy := Natural'Max (@, Send_Beam_Through (Input_Grid, R, Columns'First, Down));
            Max_Energy := Natural'Max (@, Send_Beam_Through (Input_Grid, R, Columns'Last, Down));
         end if;
      end loop;

      return Max_Energy;
   end Max_Number_Of_Energized_Tiles;

end AOC2023_16;