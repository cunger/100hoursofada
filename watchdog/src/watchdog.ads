-----------------------------------------------------------
-- Watchdog
-----------------------------------------------------------
with Ada.Real_Time; use Ada.Real_Time;

package Watchdog is

   type Callback is access procedure;

   procedure Start_With (
      Promised_Ping_Interval : Time_Span;
      Reboot_Callback : Callback
   );
   -- Starts the watchdog with an expected ping interval and
   -- a callback to reboot the caller if it fails to ping.

   procedure Ping;
   -- Lets the watchdog know that everything is fine.

private

   Expected_Ping_Interval : Time_Span;
   -- The application sets the max interval in which it promises to ping the watchdog.

   Reboot_Watched_Application : Callback;
   -- The application provides a callback that is called when it fails to ping.

   task Run is
      entry Start;
      entry Ping;
   end Run;
   -- The watchdog itself is a task, that expects pings from the application,
   -- each within in the promised interval.
   -- If the application fails to ping, the watchdog calls the reboot callback.
end Watchdog;