-----------------------------------------------------------
-- Watchdog
-----------------------------------------------------------
with Ada.Real_Time; use Ada.Real_Time;

package Watchdog is

   Cycle : constant Time_Span := Milliseconds (1000);
   Max_Cycles : constant Positive := 10;

   -- Includes a timer that counts down to zero and then raises an exception.
   -- Every ime it receives a ping from the application, the timer is reset.
   task Run is
      entry Ping;
   end Run;

   No_Ping_Received_In_Expected_Time_Span : exception;
end Watchdog;