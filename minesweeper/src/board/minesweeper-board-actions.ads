package Minesweeper.Board.Actions is

   procedure Toggle_Flag (C : in out Cell)
      with (Pre => C.Is_Hidden);

   procedure Reveal (C : in out Cell)
      with (Pre => C.Is_Hidden);

end Minesweeper.Board.Actions;