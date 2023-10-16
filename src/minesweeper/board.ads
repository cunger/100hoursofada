package Board is
 
   type Width is range 1 .. 30;
   type Height is range 1 .. 30;
   type Number_Of_Mines is range 1 .. 99;

   type Cell is record
      Number_of_Adjacent_Mines : range 1 .. 8;
      Is_Mined : Boolean := false;
      Is_Hidden : Boolean := true;
      Is_Flagged : Boolean := false;
   end record;

   type Board is array (Width, Height) of Cell;

end Board;