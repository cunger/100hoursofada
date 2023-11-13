-----------------------------------------------------------
-- Watchdog
-----------------------------------------------------------
with Ada.Real_Time; use Ada.Real_Time;

package Watchdog is

   procedure Start_With (Promised_Ping_Interval : Time_Span);
   -- Starts the watchdog with an expected ping interval.

   procedure Ping;
   -- Call this to ping the watchdog, indicating that everything is fine.

private
   -- Includes a timer that counts down to zero and then raises an exception.
   -- Every ime it receives a ping from the application, the timer is reset.
   task Run is
      entry Start_With (Promised_Ping_Interval : Time_Span);
      entry Ping;
   end Run;
   -- TODO Still missing: procedure pointer what to call for a restart
   -- when the application fails to ping
end Watchdog;