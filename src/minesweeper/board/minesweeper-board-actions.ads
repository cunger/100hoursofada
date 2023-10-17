package Minesweeper.Board.Actions is

   procedure Toggle_Flag (C : in out Cell);
   --  Pre:  cell is hidden
   --  Post: cell flagged changed

   procedure Reveal (C : in out Cell);
   --  Pre:  cell is hidden
   --  Post: cell is not hidden

end Minesweeper.Board.Actions;