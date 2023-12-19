with Ada.Containers.Vectors;
with Ada.Text_IO;

with Hash; use Hash;

package Lens_Arrangement is

   -- Takes an arrangement step and performs it.
   procedure Perform_Step (Step : in String);

   -- Sums the total focusing power of all lenses.
   function Focusing_Power return Natural;

private

   subtype Digit is Natural range 1 .. 9;
   package Digit_IO is new Ada.Text_IO.Integer_IO (Digit);

   subtype Label_String is String (1 .. 8);

   -- Extends a label to the expected fixed length of 8 by appending spaces.
   function To_Fixed_Size (From : in String) return Label_String
      with Pre => From'Length <= Label_String'Last;

   type Lens is record
      Label : Label_String;
      Focal : Digit;
   end record;

   package Lens_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Lens
   );

   type Boxes is array (Hash_Value'Range) of Lens_Vectors.Vector;

   -- Finds the index of the lens with the given label in the given box.
   -- Returns 0 if the box contains no lens with that label.
   function Find_Index (Box_No : in Hash_Value; Label : in Label_String) return Natural;

   -- Removes the lens with the given label from the given box (if it's in there).
   procedure Remove (Box_No : in Hash_Value; Label : in Label_String);

   -- Updates the focal length of the lens with the given label in he given box.
   -- If it's not contained in the box zet, it is appended.
   procedure Upsert (Box_No : in Hash_Value; Label : in Label_String; Focal_Length : in Digit);

end Lens_Arrangement;