package Minesweeper.Board.Actions is

   function Is_Hidden (C : Cell) return Boolean;

   procedure Toggle_Flag (C : in out Cell)
      with Pre => Is_Hidden (C);

   procedure Reveal (C : in out Cell)
      with Pre => Is_Hidden (C);

end Minesweeper.Board.Actions;