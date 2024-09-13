with Ada.Numerics.Generic_Elementary_Functions;

package NBodySystem is

   type Real is digits 15;

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Real);

   procedure Initialize;

   procedure Advance (D_t : Real);

   function Energy return Real;

private

   Pi            : constant Real := 3.141592653589793;
   Solar_Mass    : constant Real := 4.0 * Pi * Pi;
   Days_Per_Year : constant Real := 365.24;

   type Planet is record
       -- Position
       x    : Real;
       y    : Real;
       z    : Real;
       -- Velocity
       vx   : Real;
       vy   : Real;
       vz   : Real;
       -- Mass
       mass : Real;
   end record;

   Sun : Planet := (
      x    => 0.0,
      y    => 0.0,
      z    => 0.0,
      vx   => 0.0,
      vy   => 0.0,
      vz   => 0.0,
      mass => Solar_Mass
   );

   Jupiter : Planet := (
      x    =>  4.84143144246472090e+00,
      y    => -1.16032004402742839e+00,
      z    => -1.03622044471123109e-01,
      vx   =>  1.66007664274403694e-03 * Days_Per_Year,
      vy   =>  7.69901118419740425e-03 * Days_Per_Year,
      vz   => -6.90460016972063023e-05 * Days_Per_Year,
      mass =>  9.54791938424326609e-04 * Solar_Mass
   );

   Saturn : Planet := (
      x    =>  8.34336671824457987e+00,
      y    =>  4.12479856412430479e+00,
      z    => -4.03523417114321381e-01,
      vx   => -2.76742510726862411e-03 * Days_Per_Year,
      vy   =>  4.99852801234917238e-03 * Days_Per_Year,
      vz   =>  2.30417297573763929e-05 * Days_Per_Year,
      mass =>  2.85885980666130812e-04 * Solar_Mass
   );

   Uranus : Planet := (
      x    =>  1.28943695621391310e+01,
      y    => -1.51111514016986312e+01,
      z    => -2.23307578892655734e-01,
      vx   =>  2.96460137564761618e-03 * Days_Per_Year,
      vy   =>  2.37847173959480950e-03 * Days_Per_Year,
      vz   => -2.96589568540237556e-05 * Days_Per_Year,
      mass =>  4.36624404335156298e-05 * Solar_Mass
   );

   Neptune : Planet := (
      x    =>  1.53796971148509165e+01,
      y    => -2.59193146099879641e+01,
      z    =>  1.79258772950371181e-01,
      vx   =>  2.68067772490389322e-03 * Days_Per_Year,
      vy   =>  1.62824170038242295e-03 * Days_Per_Year,
      vz   => -9.51592254519715870e-05 * Days_Per_Year,
      mass =>  5.15138902046611451e-05 * Solar_Mass
   );

end NBodySystem;