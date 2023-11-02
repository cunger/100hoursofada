------------------------------------------------------------
-- This package defines the actions a user can take
-- on a given game board.
------------------------------------------------------------
package Minesweeper.Boards.Actions is

   -- The user can flag a cell as being mined,
   -- and can also remove that flag again.
   -- When a cell is flagged, it cannot be stepped on.
   procedure Toggle_Flag (C : in out Cell)
   with
      Pre  => Is_Hidden (C),
      Post => Is_Flagged (C) /= Is_Flagged (C'Old);

   -- A user can step on a cell, which reveals the cell
   -- and possibly adjacent parts of the board.
   -- If the cell contains a mine, it will explode
   -- (which means game over).
   procedure Reveal (C : in out Cell)
   with
      Pre =>  not Is_Flagged (C),
      Post => not Is_Hidden (C);

end Minesweeper.Boards.Actions;