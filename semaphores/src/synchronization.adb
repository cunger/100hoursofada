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

      procedure Release_After (Interval : Time_Span) is
      begin
         Reset_Interval := Interval;
      end Release_After;

      entry Acquire when Remaining_Number_Of_Executions > 0 is
      begin
         -- Interval time starts counting when the first lock is acquired.
         if Remaining_Number_Of_Executions = Max_Number_Of_Executions then
            Set_Handler (
               Reset_Interval_Has_Passed,
               Clock + Reset_Interval,
               Release_All'Access
            );
         end if;

         Remaining_Number_Of_Executions := @ - 1;
      end Acquire;

      procedure Release_All is
      begin
         Remaining_Number_Of_Executions := Max_Number_Of_Executions;
      end Release_All;

   end Timed_Semaphore;

end Synchronization;