with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

procedure Main is

   -- Empty procedures that pretend to do some work.
   procedure Read_Sensor is null;
   procedure Check_Status is null;

   Processing_Cycle : Natural := 0;

   -- Pretend that our work is building up slag,
   -- until a defined breakpoint where it hangs.
   Slag_Buildup : Natural := 0;
   Slag_Breakpoint : constant Positive := 10;

   procedure Reset;
   procedure Reset is
   begin
      Slag_Buildup := 0;
   end Reset;

   -- Watchdog task

   type Callback is access procedure;

   task Watchdog is
      entry Start (
         Promised_Ping_Interval : Time_Span;
         Timeout_Callback : Callback
      );

      entry Ping;
      entry Stop;
   end Watchdog;

   task body Watchdog is

      Expected_Ping_Interval : Time_Span;
      -- The application sets the max interval in which it promises to ping the watchdog.

      Application_Callback : Callback;
      -- The application provides a callback that is called when it fails to ping.

      Last_Ping : Time;
      -- Keeping track of when the last ping was received.

   begin
      -- First wait for the application to start the watchdog.
      accept Start (
         Promised_Ping_Interval : Time_Span;
         Timeout_Callback : Callback
      ) do
         Expected_Ping_Interval := Promised_Ping_Interval;
         Application_Callback := Timeout_Callback;

         Ada.Text_IO.Put_Line ("Watching!");

         Last_Ping := Clock;
      end Start;

      -- Once started, wait for pings in the expected time interval.
      Wait : loop
         select
            -- If there's a ping, all is fine.
            accept Ping;
            Last_Ping := Clock;

            Ada.Text_IO.Put_Line ("Received ping.");
         or
            accept Stop;
            exit Wait;
         or
            -- If there is no ping within the expected time interval,
            -- reboot the watched application.
            delay until (Last_Ping + Expected_Ping_Interval);

            Ada.Text_IO.Put_Line ("Timeout!");

            Application_Callback.all;
            Last_Ping := Clock;
         end select;
      end loop Wait;
   end Watchdog;
begin

   -- Start the watchdog, expecting a regular ping within one second.
   Watchdog.Start (
      Promised_Ping_Interval => Milliseconds (1000),
      Timeout_Callback => Reset'Access
   );

   Processing : loop
      exit Processing when Processing_Cycle = 3;

      -- Check the slag buildup. If we reached a critical level,
      -- simulate that the processing hangs and doesn't do anything anymore.
      if Slag_Buildup >= Slag_Breakpoint then
         delay 4.0;
         Processing_Cycle := Processing_Cycle + 1;
      end if;

      Read_Sensor;
      Check_Status;

      Slag_Buildup := Slag_Buildup + 1;

      Watchdog.Ping;

      delay 0.6;
   end loop Processing;

   Watchdog.Stop;
end Main;
