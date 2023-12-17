--------------------------------------
-- https://adventofcode.com/2023/day/4
--------------------------------------
with Ada.Text_IO;

package AOC2023_04 is

   -- Part 1
   function Total_Number_Of_Points return Natural;

   -- Part 2
   function Total_Number_Of_Cards return Natural;

private

   Input_File_Name : constant String := "src/04/input_04.txt";

   -- For storing the winning numbers of a card, use an array that specifies
   -- for each number in the range whether it appears on the card as winning
   -- number or not. (Idea stolen from John Perry.)
   type Number  is range 0 .. 99;
   type Numbers is array (Number) of Boolean;

   package Num_IO is new Ada.Text_IO.Integer_IO (Number);

   -- For storing how many matching numbers each card has.
   type Card_Pile is array (1 .. 211) of Natural;

end AOC2023_04;