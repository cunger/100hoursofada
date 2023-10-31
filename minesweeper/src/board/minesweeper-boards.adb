package body Minesweeper.Boards is

   function Is_Mined (C : Cell) return Boolean is
   begin
      return C.Mined;
   end Is_Mined;

   function Is_Flagged (C : Cell) return Boolean is
   begin
      return C.Flagged;
   end Is_Flagged;

   function Is_Hidden (C : Cell) return Boolean is
   begin
      return not C.Visible;
   end Is_Hidden;

end Minesweeper.Boards;