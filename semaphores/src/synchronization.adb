package body Synchronization is

   protected body Semaphore is

      entry Acquire when Remaining_Number_Of_Executions > 0 is
      begin
         Remaining_Number_Of_Executions := @ - 1;
      end Acquire;

      procedure Release is
      begin
         Remaining_Number_Of_Executions := Natural'Min (@ + 1, Max_Number_Of_Executions);
         -- Remaining_Number_Of_Executions is bounded by the given maximum,
         -- because otherwise you could generate more possible executions
         -- by simply calling Release often enough.
      end Release;

   end Semaphore;

   protected body Timed_Semaphore is

      entry Acquire when Remaining_Number_Of_Executions > 0 is
      begin
         -- The time interval starts counting when the first lock is acquired.
         -- When it is over, all locks are released.
         if Remaining_Number_Of_Executions = Max_Number_Of_Executions then
            Set_Handler (
               Event   => Release_Interval_Has_Passed,
               In_Time => Release_Interval,
               Handler => Release_All'Access
            );
         end if;

         Remaining_Number_Of_Executions := @ - 1;
      end Acquire;

      procedure Release_All (Event : in out Timing_Event) is
         pragma Unreferenced (Event);
      begin
         Remaining_Number_Of_Executions := Max_Number_Of_Executions;
      end Release_All;

   end Timed_Semaphore;

end Synchronization;