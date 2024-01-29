with Ada.Real_Time; use Ada.Real_Time;
with Ada.Real_Time.Timing_Events; use Ada.Real_Time.Timing_Events;

package Synchronization is

   -- A semaphore is a synchronization primitive that controls
   -- the number of locks that are available at any one time.
   --
   -- In order to define a semaphore, specify the number of
   -- processes that you want to allow in parallel,
   -- for example:
   --
   -- S : Semaphore (10);
   --
   -- Each process should acquire a lock before running,
   -- and release that lock when it's done:
   --
   -- S.Acquire;
   -- ...
   -- S.Release;
   protected type Semaphore (Max_Number_Of_Executions : Positive) is

      -- Acquire a lock. If none is free currently, you will
      -- be queued until there is one.
      entry Acquire;

      -- Release the lock again, so other processes can use it.
      procedure Release;

   private

      Remaining_Number_Of_Executions : Natural := Max_Number_Of_Executions;

   end Semaphore;

   -- A timed semaphore is a synchronization primitive that controls
   -- the number of locks that are available within a specific time span.
   --
   -- In order to define a timed semaphore, specify a time interval and
   -- the number of processes that you want to allow within that interval,
   -- for example:
   --
   -- S : Time_Semaphore (10, Within_Seconds => 60);
   --
   -- Each process should acquire a lock before running:
   --
   -- S.Acquire;
   -- ...
   --
   -- The lock is released automatically after the specified interval.
   protected type Timed_Semaphore (
      Max_Number_Of_Executions : Positive;
      Within_Seconds : Positive
   ) is

      -- Acquire a lock. If none is free currently, you will
      -- be queued until there is one.
      entry Acquire;

   private

      Remaining_Number_Of_Executions : Natural := Max_Number_Of_Executions;

      Release_Interval : Time_Span := Seconds (Within_Seconds);
      Release_Interval_Has_Passed : Timing_Event;

      procedure Release_All (Event : in out Timing_Event);

   end Timed_Semaphore;

end Synchronization;