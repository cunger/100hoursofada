with Util.Log.Loggers;

-- Note: This is work in progress; really just tinkering at the moment.
package body Watchdog is
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("watchdog");

   ---------------------------------------------------------------
   -- Interface for applications to start and ping the watchdog --
   ---------------------------------------------------------------

   procedure Start_With (Promised_Ping_Interval : Time_Span) is
   begin
      Run.Start_With (Promised_Ping_Interval);
   end Start_With;

   procedure Ping is
   begin
      Run.Ping;
   end Ping;

   -------------------------
   -- Task implementation --
   -------------------------

   task body Run is

      Expected_Ping_Interval : Time_Span;
      -- The application sets the max interval in which it promises to ping the watchdog.

      Last_Ping : Time;
      -- The watchdog keeps track of when it received the last ping.

   begin
      -- First wait for the application to start the watchdog with an interval.
      accept Start_With (Promised_Ping_Interval : Time_Span) do
         Expected_Ping_Interval := Promised_Ping_Interval;

         Log.Info ("Starting watchdog timer...");

         Last_Ping := Clock;
      end Start_With;

      Wait : loop
         select
            -- Wait for pings. When they come, reset Last_Ping.
            accept Ping do
               Log.Debug ("Received ping."); -- will be deactivated to avoid overhead
               Last_Ping := Clock;
            end Ping;
         or
            -- If there is no ping within the expected interval, reboot the process.
            delay until (Last_Ping + Expected_Ping_Interval);
            Log.Error ("Timeout! Will reboot the process...");
            exit Wait;
         end select;
      end loop Wait;
   end Run;
end Watchdog;