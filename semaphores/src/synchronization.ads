with Ada.Real_Time; use Ada.Real_Time;
with Ada.Real_Time.Timing_Events; use Ada.Real_Time.Timing_Events;

package Synchronization is

   -- TODO exaplanation
   --
   -- Example:
   --
   -- S : Semaphore (10);
   --
   -- S.Acquire;
   -- [your computation]
   -- S.Release;
   protected type Semaphore (Max_Number_Of_Executions : Positive) is

      -- Before starting the computation you want to secure,
      -- acquire a lock. If none is free currently, you will
      -- be queued until there is one.
      entry Acquire;

      -- After finishing the computation you want to secure,
      -- release the lock again.
      procedure Release;

   private

      Remaining_Number_Of_Executions : Natural := Max_Number_Of_Executions;

   end Semaphore;


   Interval_Not_Set : exception;

   protected type Timed_Semaphore (Max_Number_Of_Executions : Positive) is

      entry Acquire;

      procedure Release_After (Interval : Time_Span);

   private

      Reset_Interval : Time_Span := Seconds (60);
      Reset_Interval_Has_Passed : Timing_Event;

      Remaining_Number_Of_Executions : Natural := Max_Number_Of_Executions;

      procedure Release_All;

   end Timed_Semaphore;

end Synchronization;