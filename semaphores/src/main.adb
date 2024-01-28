with Ada.Real_Time; use Ada.Real_Time;
with Synchronization; use Synchronization;

procedure Main is

   S : Semaphore (4);
   T : Timed_Semaphore (4);

begin

   T.Release_After (Seconds (10));

   S.Acquire;

end Main;
