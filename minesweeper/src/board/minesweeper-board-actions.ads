package Minesweeper.Board.Actions is

   procedure Toggle_Flag (C : in out Cell)
      with Pre => Is_Hidden (C);

   procedure Reveal (C : in out Cell)
      with Pre => Is_Hidden (C);

   function Is_Hidden (C : Cell) return Boolean;

end Minesweeper.Board.Actions;