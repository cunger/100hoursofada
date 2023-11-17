with Simulation;
with Util.Log.Loggers;

procedure Main is
begin
   Util.Log.Loggers.Initialize ("log4j.properties");

   Simulation.Setup;
   Simulation.Start_Processing;

   -- TODO After some runs, shutdown simulation
   -- and check that the wathdog terminates as well.
end Main;
