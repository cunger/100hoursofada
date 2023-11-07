-----------------------------------------------------------
-- Watchdog
-----------------------------------------------------------
with Ada.Real_Time; use Ada.Real_Time;

package Watchdog is

   -- TODO needs to be configurable
   -- TODO also specify timespan directly instead of cycles
   Cycle : constant Time_Span := Milliseconds (1000);
   Max_Cycles : constant Positive := 10;

   -- TODO Still missing: procedure pointer what to call for a restart
   -- when the application fails to ping

   -- Includes a timer that counts down to zero and then raises an exception.
   -- Every ime it receives a ping from the application, the timer is reset.
   task Run is
      entry Ping;
   end Run;

   procedure Ping;
   -- Call this to ping the watchdog, indicating that everything is fine.

   Timeout : exception;
end Watchdog;