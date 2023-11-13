with Watchdog;
with Ada.Real_Time;
with Util.Log.Loggers;

package body Simulation is
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("simulation");

   -- Pretending the procedure is building up slag,
   -- until a defined breakpoint where it hangs.
   Slag_Buildup : Natural := 0;
   Slag_Breakpoint : constant Positive := 10;

   procedure Read_Sensor is null;
   procedure Check_Status is null;

   procedure Start is
   begin
      Log.Info ("Starting simulation... (cancel it with Ctrl+C)");

      Watchdog.Start_With (Promised_Ping_Interval => Ada.Real_Time.Milliseconds (1000));

      -- Example loop where each iteration needs to complete within a set amount of time
      Processing : loop
         -- Check the slag buildup. If we reached a critical level,
         -- simulate that the processing hangs and doesn't do anything anymore.
         -- If it's not rebooted, it simply exits.
         if Slag_Buildup >= Slag_Breakpoint then
            Log.Warn ("Oops, hanging...");
            delay 10.0;
            exit Processing;
         end if;

         Read_Sensor;
         Check_Status;

         Slag_Buildup := Slag_Buildup + 1;

         Watchdog.Ping;

         delay 0.6;
      end loop Processing;
   end Start;

   procedure Reboot is
   begin
      Slag_Buildup := 0;
      Start;
   end Reboot;

end Simulation;