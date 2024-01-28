with Ada.Text_IO;
with Synchronization; use Synchronization;

procedure Main is

   S : Semaphore (5);
   -- allows for max 5 executions at any one time

   T : Timed_Semaphore (10, Within_Seconds => 60);
   -- allows for max 5 executions within one minute

   task type Execution;
   task type Timed_Execution;

   task body Execution is
   begin
      S.Acquire;

      -- Pretending to do some work.
      Ada.Text_IO.Put_Line ("Starting execution with semaphore lock...");
      delay 4.0;
      Ada.Text_IO.Put_Line ("Finished execution with semaphore lock...");

      S.Release;
   end Execution;

   task body Timed_Execution is
   begin
      T.Acquire;

      -- Pretending to do some work.
      Ada.Text_IO.Put_Line ("Starting execution with timed semaphore lock...");
      delay 2.0;
      Ada.Text_IO.Put_Line ("Finished execution with timed semaphore lock...");
   end Timed_Execution;

   Executions       : array (1 .. 8)  of Execution;
   Timed_Executions : array (1 .. 12) of Timed_Execution;

begin
   null;
end Main;
