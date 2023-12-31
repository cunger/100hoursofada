with Ada.Exceptions;
with Util.Log.Loggers;

-- Note: This is work in progress; really just tinkering at the moment.
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
      Reboot_Watched_Application := Reboot_Callback;

      Timer.Start;
   end Start_With;

   procedure Ping is
   begin
      Timer.Ping;
   end Ping;

   -------------------------
   -- Task implementation --
   -------------------------

   task body Timer is

      Last_Ping : Time; -- Keeping track of when the last ping was received.

   begin
      -- First wait for the application to start the watchdog.
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
            -- If there is no ping within the expected interval, reboot the process.
            delay until (Last_Ping + Expected_Ping_Interval);
            Log.Error ("Timeout! Rebooting the application...");
            Reboot_Watched_Application.all;
            Last_Ping := Clock;
         end select;
      end loop Wait;
   exception
      when E : Tasking_Error | Program_Error | Constraint_Error =>
         Log.Error ("Exception: " & Ada.Exceptions.Exception_Message (E));
   end Timer;
end Watchdog;