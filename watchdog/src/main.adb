with Simulation;
with Util.Log.Loggers;

procedure Main is
begin
   Util.Log.Loggers.Initialize ("log4j.properties");

   Simulation.Start;
end Main;
