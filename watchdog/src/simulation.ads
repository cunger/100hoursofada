-------------------------------------------------------------------------------
-- This package is for simulating a process that runs a work loop
-- which needs to finish within a certain amount of time.
-- To ensure this timing constraint, it regularly pings the watchdog,
-- which will reboot the process if it hangs and fails to send a ping.
-------------------------------------------------------------------------------
package Simulation is

   procedure Start;
   -- Starts simulating a processing loop, which builds up slag over time
   -- and at some point will hang.

   procedure Reboot;
   -- Restarts the processing loop with no slag.

end Simulation;