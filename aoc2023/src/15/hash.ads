package Hash is

   subtype Hash_Value is Natural range 0 .. 255;

   function Hash_256 (Str : in String) return Hash_Value;

end Hash;