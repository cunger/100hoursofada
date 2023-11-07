with Watchdog;
with Ada.Text_IO;

package body Simulation is

   procedure Print (Line : String) renames Ada.Text_IO.Put_Line;

   -- Pretending the procedure is building up slag,
   -- until a defined breakpoint where it hangs.
   Slag_Buildup : Natural := 0;
   Slag_Breakpoint : constant Positive := 10;

   procedure Read_Sensor is null;
   procedure Check_Status is null;

   procedure Start is
   begin
      Print ("Starting simulation... (cancel it with Ctrl+C)");

      -- Example loop where each iteration needs to complete within a set amount of time
      Processing : loop
         -- Check the slag buildup. If we reached a critical level,
         -- simulate that the processing hangs and doesn't do anything anymore.
         -- If it's not rebooted, it simply exits.
         if Slag_Buildup >= Slag_Breakpoint then
            Print ("Oops, hanging...");
            delay 10.0;
            exit Processing;
         end if;

         Read_Sensor;
         Check_Status;
         Print ("All fine!");

         Slag_Buildup := Slag_Buildup + 1;

         Watchdog.Ping;

         delay 1.0;
      end loop Processing;
   end Start;

   procedure Reboot is
   begin
      Slag_Buildup := 0;
      Start;
   end Reboot;

end Simulation;