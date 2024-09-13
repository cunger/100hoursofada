package body NBodySystem is

   Planets : array (1 .. 5) of Planet := [
      Sun,
      Jupiter,
      Saturn,
      Uranus,
      Neptune
   ];

   procedure Initialize is
      Px : Real := 0.0;
      Py : Real := 0.0;
      Pz : Real := 0.0;
   begin
      for P of Planets loop
         Px := @ + P.vx * P.mass;
         Py := @ + P.vy * P.mass;
         Pz := @ + P.vz * P.mass;
      end loop;

      -- Offset momentum of the sun
      Sun.vx := -Px / Solar_Mass;
      Sun.vy := -Py / Solar_Mass;
      Sun.vz := -Pz / Solar_Mass;
   end Initialize;

   procedure Advance (Dt : Real) is
      Dx : Real;
      Dy : Real;
      Dz : Real;
      Sum_Of_Squares : Real;
      Distance : Real;
      Magnitude : Real;
   begin
      for I in Planets'Range loop
         for J in I + 1 .. Planets'Last loop
            Dx := Planets (I).x - Planets (J).x;
            Dy := Planets (I).y - Planets (J).y;
            Dz := Planets (I).z - Planets (J).z;

            Sum_Of_Squares := Dx * Dx + Dy * Dy + Dz * Dz;
            Distance  := Math.Sqrt (Sum_Of_Squares);
            Magnitude := Dt / (Sum_Of_Squares * Distance);

            Planets (I).vx := @ - Dx * Planets (J).mass * Magnitude;
            Planets (I).vy := @ - Dy * Planets (J).mass * Magnitude;
            Planets (I).vz := @ - Dz * Planets (J).mass * Magnitude;

            Planets (J).vx := @ + Dx * Planets (I).mass * Magnitude;
            Planets (J).vy := @ + Dy * Planets (I).mass * Magnitude;
            Planets (J).vz := @ + Dz * Planets (I).mass * Magnitude;
         end loop;
      end loop;

      for I in Planets'Range loop
         Planets (I).x := @ + Dt * Planets (I).vx;
         Planets (I).y := @ + Dt * Planets (I).vy;
         Planets (I).z := @ + Dt * Planets (I).vz;
      end loop;
   end Advance;

   function Energy return Real is
      E : Real := 0.0;

      Dx : Real;
      Dy : Real;
      Dz : Real;
      Distance : Real;
   begin
      for I in Planets'Range loop
         -- Kinetic energy of the body.
         E := @ + 0.5 * Planets (I).mass * (
            Planets (I).vx * Planets (I).vx +
            Planets (I).vy * Planets (I).vy +
            Planets (I).vz * Planets (I).vz
         );

         -- Potential energy between pairs of bodies.
         for J in I + 1 .. Planets'Last loop
            Dx := Planets (I).x - Planets (J).x;
            Dy := Planets (I).y - Planets (J).y;
            Dz := Planets (I).z - Planets (J).z;

            Distance := Math.Sqrt (Dx * Dx + Dy * Dy + Dz * Dz);

            E := @ - (Planets (I).mass * Planets (J).mass) / Distance;
         end loop;
      end loop;

      return E;
   end Energy;

end NBodySystem;