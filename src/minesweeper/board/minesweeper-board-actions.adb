package body Minesweeper.Board.Actions is

   procedure Toggle_Flag (C : in out Cell) is
   begin
      C.Is_Flagged := not C.Is_Flagged;
   end Toggle_Flag;

   procedure Reveal (C : in out Cell) is
   begin
      C.Is_Hidden := False;
   end Reveal;

end Minesweeper.Board.Actions;