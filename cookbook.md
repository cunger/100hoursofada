
## Iterating over all characters in a string

```ada
declare
   Str  : constant String := "Fnord";
   Char : Character;
begin
   for I in Str'Range loop
      Char := Str (I);
   end loop;
end;
```

## Matching a regex

```ada
with GNAT.Regpat; use GNAT.Regpat;

declare
   Pattern : constant Pattern_Matcher := Compile ("(\d+)x(\d+)x(\d+)");
   Example : constant String := "10x2x3";
   Matches : Match_Array (0 .. 3);
   Length  : Natural;
   Width   : Natural;
   Height  : Natural;
begin
   Match (Pattern, Line, Matches);
   if Matches (0) /= No_Match then
      Length := Natural'Value (Example (Matches (1).First .. Matches (1).Last));
      Width  := Natural'Value (Example (Matches (2).First .. Matches (2).Last));
      Height := Natural'Value (Example (Matches (3).First .. Matches (3).Last));
   end if;
end;
```