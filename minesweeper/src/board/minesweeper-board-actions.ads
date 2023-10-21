package Minesweeper.Board.Actions is

   procedure Toggle_Flag (C : in out Cell)
      with Pre => Is_Hidden (C);

   procedure Reveal (C : in out Cell)
      with Pre => Is_Hidden (C);

end Minesweeper.Board.Actions;