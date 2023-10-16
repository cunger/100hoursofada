package Board is

   -- Board settings
   
   type Width is range 1 .. 30;
   type Height is range 1 .. 30;
   type Number_Of_Mines is range 1 .. 99;

   -- Definition of a board and its cells

   type Cell is record
      Number_of_Adjacent_Mines : range 1 .. 8;
      Is_Mined : Boolean := false;
      Is_Hidden : Boolean := true;
      Is_Flagged : Boolean := false;
   end record;

   type Board is array (Width, Height) of Cell;

   -- Generating a board

   function Generate_Empty_Board (Width, Height) return Board;
   
   procedure Place_Mines (Board, Number_Of_Mines);
   procedure Calculate_Markers (Board);

   -- Actions on board cells

   procedure Toggle_Flag (Cell);
   -- Pre:  cell is hidden
   -- Post: if cell was not flagged, it is flagged, else it is not flagged

   procedure Reveal (Cell);
   -- Pre:  cell is hidden
   -- Post: cell is not hidden

end Board;