package Minesweeper.Board.Actions is

   procedure Toggle_Flag (C : in out Cell)
      with Pre  => Is_Hidden (C),
           Post => Is_Flagged (C) /= Is_Flagged (C'Old);

   procedure Reveal (C : in out Cell)
      with Pre =>  Is_Hidden (C),
           Post => not Is_Hidden (C);

end Minesweeper.Board.Actions;