with Watchdog;
with Ada.Real_Time;
with Util.Log.Loggers;

package body Simulation is
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("simulation");

   -- Pretending the procedure is building up slag,
   -- until a defined breakpoint where it hangs.
   Slag_Buildup : Natural := 0;
   Slag_Breakpoint : constant Positive := 10;

   -- Empty procedures that pretend to do some work.
   procedure Read_Sensor is null;
   procedure Check_Status is null;

   procedure Setup is
   begin
      Log.Info ("All set up. Starting watchdog...");

      Watchdog.Start_With (
         Promised_Ping_Interval => Ada.Real_Time.Milliseconds (1000),
         Reboot_Callback => Reboot'Access
      );
   end Setup;

   procedure Start_Processing is
   begin
      Log.Info ("Starting processing... (cancel with Ctrl+C)");

      -- Processing loop. Each iteration needs to complete within a set amount of time.
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
   end Start_Processing;

   procedure Reboot is
   begin
      Log.Info ("Reboot...");

      Slag_Buildup := 0;
      Start_Processing;
   end Reboot;

end Simulation;