package body Minesweeper.Board.Actions is

   procedure Toggle_Flag (C : in out Cell) is
   begin
      C.Flagged := not C.Flagged;
   end Toggle_Flag;

   procedure Reveal (C : in out Cell) is
   begin
      C.Visible := True;
   end Reveal;

   function Is_Hidden (C : Cell) return Boolean is
   begin
      return not C.Visible;
   end Is_Hidden;

end Minesweeper.Board.Actions;