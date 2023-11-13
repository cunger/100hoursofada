with Util.Log.Loggers;

-- Note: This is work in progress; really just tinkering at the moment.
package body Watchdog is
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("watchdog");

   -- Main task of the watchdog: Wait for pings, and if they don't come,
   -- reboot the process.
   task body Run is
      Elapsed_Cycles : Natural := 0;
   begin
      Log.Info ("Starting...");

      loop
         if Elapsed_Cycles = Max_Cycles then
            raise Timeout;
         end if;

         select
            accept Ping do
               Elapsed_Cycles := 0;
               Log.Debug ("Received ping.");
            end Ping;
         else
            delay until (Clock + Cycle);
            Elapsed_Cycles := Elapsed_Cycles + 1;
         end select;
      end loop;
   exception
      when Timeout =>
         Log.Warn ("Timeout! Will reboot the process...");
         -- TODO restart the application
   end Run;

   procedure Ping is
   begin
      Run.Ping;
   end Ping;
end Watchdog;