package body Watchdog is
   task body Run is
      Elapsed_Cycles : Natural := 0;
   begin
      loop
         if Elapsed_Cycles = Max_Cycles then
            raise No_Ping_Received_In_Expected_Time_Span;
         end if;

         select
            accept Ping do
               Elapsed_Cycles := 0;
            end Ping;
         else
            delay until (Clock + Cycle);
            Elapsed_Cycles := Elapsed_Cycles + 1;
         end select;
      end loop;
   end Run;
end Watchdog;