package Board.Actions is

   procedure Toggle_Flag (Cell);
   -- Pre:  cell is hidden

   procedure Reveal (Cell);
   -- Pre:  cell is hidden
   -- Post: cell is not hidden

end Board.Actions;