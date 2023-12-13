pragma Ada_2022;

with Ada.Text_IO;
with Ada.Strings;

package body AOC2023_08 is

   -- Parse the input file.
   -- Expects an empty instructions vector and an empty network,
   -- both of which are filled based on the input.
   procedure Process_Input (
      Nav : in out Instructions;
      Map : in out Network
   );

   procedure Parse_Instructions (Line : String; Parsed : in out Instructions);
   procedure Parse_Branch       (Line : String; Parsed : in out Network);

   -- Traverses the given network from start to destination,
   -- following the given instructions.
   -- Returns the number of steps required for the traversal.
   function Traverse (
      Navigation  : Instructions;
      Map         : Network;
      Start       : Node;
      Destination : Node
   ) return Natural
      with Pre => not Is_Empty (Navigation);

   -- Part 1: Count the number of steps needed to traverse
   -- the wasteland from "AAA" to "ZZZ".
   function Steps_From_AAA_To_ZZZ return Natural is
      Navigation : Instructions;
      Wasteland  : Network;
      Steps      : Natural;
   begin
      Process_Input (Navigation, Wasteland);

      Steps := Traverse (
         Navigation  => Navigation,
         Map         => Wasteland,
         Start       => "AAA",
         Destination => "ZZZ"
      );

      return Steps;
   end Steps_From_AAA_To_ZZZ;

   function Traverse (
      Navigation  : Instructions;
      Map         : Network;
      Start       : Node;
      Destination : Node
   ) return Natural is
      Next_Instruction : Direction;
      Nav_Cursor       : Cursor  := First (Navigation);
      Position         : Node    := Start;
      Steps            : Natural := 0;
   begin
      while not (Position = Destination) loop
         -- Follow next instruction.
         Next_Instruction := Element (Nav_Cursor);
         case Next_Instruction is
            when Left  => Position := Map (Position).Left;
            when Right => Position := Map (Position).Right;
         end case;

         -- Count step.
         Steps := @ + 1;

         -- Forward instruction cursor.
         Nav_Cursor := Next (Nav_Cursor);
         -- If we reached the end of the instructions,
         -- then start again at the beginning.
         if not Has_Element (Nav_Cursor) then
            Nav_Cursor := First (Navigation);
         end if;
      end loop;

      return Steps;
   end Traverse;

   procedure Process_Input (Nav : in out Instructions; Map : in out Network) is
      Input : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);

      -- The first line contains the instructions.
      Parse_Instructions (Ada.Text_IO.Get_Line (Input), Nav);

      -- The second line is empty.
      Ada.Text_IO.Skip_Line (Input, 1);

      -- The remaining lines define the network.
      while not Ada.Text_IO.End_Of_File (Input) loop
         Parse_Branch (Ada.Text_IO.Get_Line (Input), Map);
      end loop;

      Ada.Text_IO.Close (Input);
   end Process_Input;

   procedure Parse_Instructions (Line : String; Parsed : in out Instructions) is
   begin
      for Index in Line'First .. Line'Last loop
         case Line (Index) is
            when 'L'    => Parsed.Append (Left);
            when 'R'    => Parsed.Append (Right);
            when others => raise Unexpected_Input_Exception;
         end case;
      end loop;
   end Parse_Instructions;

   procedure Parse_Branch (Line : String; Parsed : in out Network) is
      Parent, Left_Child, Right_Child : Node;
   begin
      Parent      := Line (Line'First      .. Line'First + 2);
      Left_Child  := Line (Line'First + 7  .. Line'First + 9);
      Right_Child := Line (Line'First + 12 .. Line'First + 14);

      Parsed.Insert (Parent, (Left_Child, Right_Child));
   end Parse_Branch;

end AOC2023_08;