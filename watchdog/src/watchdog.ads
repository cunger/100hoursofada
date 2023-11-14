-----------------------------------------------------------
-- Watchdog
-----------------------------------------------------------
with Ada.Real_Time; use Ada.Real_Time;

package Watchdog is

   procedure Start_With (Promised_Ping_Interval : Time_Span);
   -- Starts the watchdog with an expected ping interval.

   procedure Ping;
   -- Lets the watchdog know that everything is fine.

private
   -- The task is expecting pings from the application,
   -- each within in the promised interval.
   -- If the application fails to ping, the watchdog reboots it.
   task Run is
      entry Start_With (Promised_Ping_Interval : Time_Span);
      entry Ping;
   end Run;
   -- TODO Still missing: procedure pointer what to call for a restart
   -- when the application fails to ping
end Watchdog;