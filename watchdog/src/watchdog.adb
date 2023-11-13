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

      Last_Ping : Time;
      Time_Since_Last_Ping : Time_Span;
   begin
      -- First wait for the application to start the watchdog with a given timespan.
      accept Start_With (Promised_Ping_Interval : Time_Span) do
         Log.Info ("Starting...");
         Expected_Ping_Interval := Promised_Ping_Interval;
      end Start_With;

      -- Once started, initialize Last_Ping and wait.
      Last_Ping := Clock;
      loop
         -- Wait for pings.
         select
            accept Ping do
               Log.Debug ("Received ping.");
               Last_Ping := Clock;
            end Ping;
         else
            delay until (Clock + Expected_Ping_Interval);
         end select;

         -- If there was no ping within the expected interval, reboot the process.
         -- FIXME
         Time_Since_Last_Ping := Clock - Last_Ping;
         Log.Debug ("Time since last ping: " & Duration'Image (To_Duration (Time_Since_Last_Ping)));
         if Time_Since_Last_Ping > Expected_Ping_Interval then
            Log.Error ("Timeout! Will reboot the process...");
         end if;
      end loop;
   end Run;
end Watchdog;