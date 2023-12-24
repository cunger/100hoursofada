package Contraption.Energize is

   -- Function for sending a beam through the contraption.
   -- It takes the unenergized grid as input, and then sends a beam through it,
   -- starting in the cell with the given row and column coordinates and
   -- going into the given direction.
   -- Returns the number of energized cells after the beam is done.
   function Send_Beam_Through (G : Grid; Row : Rows; Col : Columns; Dir : Direction)
      return Natural;

end Contraption.Energize;