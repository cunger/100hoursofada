package body Hash is

   function Hash_256 (Str : String) return Hash_Value is
      Hash : Natural := 0;
   begin
      For_Each_Character : for Char of Str loop
         Hash := ((@ + Character'Pos (Char)) * 17) mod 256;
      end loop For_Each_Character;

      return Hash;
   end Hash_256;

end Hash;