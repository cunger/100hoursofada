with Ada.Text_IO;

-- Note: This is work in progress; really just tinkering at the moment.
package body Watchdog is

   procedure Print (Line : String) renames Ada.Text_IO.Put_Line;

   -- Main task of the watchdog: Wait for pings, and if they don't come,
   -- reboot the process.
   task body Run is
      Elapsed_Cycles : Natural := 0;
   begin
      Print ("Starting watchdog...");

      loop
         if Elapsed_Cycles = Max_Cycles then
            raise Timeout;
         end if;

         select
            accept Ping do
               Elapsed_Cycles := 0;
               Print ("Received ping.");
            end Ping;
         else
            delay until (Clock + Cycle);
            Elapsed_Cycles := Elapsed_Cycles + 1;
         end select;
      end loop;
   exception
      when Timeout =>
         Print ("Timeout! Will reboot the process...");
         -- TODO restart the application
   end Run;

   procedure Ping is
   begin
      Run.Ping;
   end Ping;
end Watchdog;