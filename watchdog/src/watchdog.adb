with Ada.Exceptions;
with Util.Log.Loggers;

-- Note: This is work in progress and more tinkering than proper working code.
package body Watchdog is
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("watchdog");

   ---------------------------------------------------------------
   -- Interface for applications to start and ping the watchdog --
   ---------------------------------------------------------------

   procedure Start_With (
      Promised_Ping_Interval : Time_Span;
      Reboot_Callback : Callback
   ) is
   begin
      Expected_Ping_Interval := Promised_Ping_Interval;
      Reboot_Application := Reboot_Callback;

      Timer.Start;
   end Start_With;

   procedure Ping is
   begin
      Timer.Ping;
   end Ping;

   -------------------------
   -- Task implementation --
   -------------------------

   -- The timer task keeps track of when the last ping was received,
   -- and calls the interface task for a reboot if there is no ping.
   task body Timer is
      Last_Ping : Time;
   begin
      accept Start do
         Log.Debug ("Watching!");
         Last_Ping := Clock;
      end Start;

      Wait : loop
         select
            -- Wait for pings. When they come, reset Last_Ping.
            accept Ping do
               Log.Debug ("Received ping.");
               Last_Ping := Clock;
            end Ping;
         or
            -- If there is no ping within the expected interval, tell the application to reboot.
            delay until (Last_Ping + Expected_Ping_Interval);
            Log.Error ("Timeout!");
            Interface_To_Application.Reboot;
         end select;
      end loop Wait;
   exception
      when E : Tasking_Error | Program_Error | Constraint_Error =>
         Log.Error ("Exception: " & Ada.Exceptions.Exception_Message (E));
   end Timer;

   -- The interface task is responsible for rebooting the application.
   task body Interface_To_Application is
   begin
      loop
         select
            accept Reboot do
               Reboot_Application.all;
            end Reboot;
         or
            terminate;
         end select;
      end loop;
   exception
      when E : Tasking_Error | Program_Error | Constraint_Error =>
         Log.Error ("Exception: " & Ada.Exceptions.Exception_Message (E));
   end Interface_To_Application;
end Watchdog;