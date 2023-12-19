with Util.Strings;

package body Lens_Arrangement is
   use Lens_Vectors;

   All_Boxes : Boxes := [others => Empty_Vector];

   procedure Perform_Step (Step : in String) is
   begin
      if Util.Strings.Ends_With (Step, "-") then
         declare
            Label  : constant String     := Step (Step'First .. (Step'Last - 1));
            Box_No : constant Hash_Value := Hash_256 (Label);
         begin
            Remove (Box_No, To_Fixed_Size (Label));
         end;
      end if;

      if Util.Strings.Contains (Step, "=") then
         declare
            Label  : constant String     := Step (Step'First .. (Step'Last - 2));
            Value  : constant String     := Step (Step'Last .. Step'Last);
            Box_No : constant Hash_Value := Hash_256 (Label);
            Focal  : Digit;
            Index  : Positive := Step'Last;
         begin
            Digit_IO.Get (From => Value, Item => Focal, Last => Index);

            Upsert (Box_No, To_Fixed_Size (Label), Focal);
         end;
      end if;
   end Perform_Step;

   function To_Fixed_Size (From : in String) return Label_String is
   begin
      return From & [(From'Length + 1) .. Label_String'Last => ' '];
   end To_Fixed_Size;

   function Find_Index (Box_No : in Hash_Value; Label : in Label_String) return Natural is
      Position   : Cursor  := First (All_Boxes (Box_No));
      Lens_Index : Natural := 0;
   begin
      For_Each_Lens : while Has_Element (Position) loop
         if Element (Position).Label = Label then
            Lens_Index := To_Index (Position);
            exit For_Each_Lens;
         else
            Position := Next (Position);
         end if;
      end loop For_Each_Lens;

      return Lens_Index;
   end Find_Index;

   procedure Remove (Box_No : in Hash_Value; Label : in Label_String) is
      Lens_Index : Natural;
   begin
      Lens_Index := Find_Index (Box_No, Label);

      if Lens_Index > 0 then
         All_Boxes (Box_No).Delete (Lens_Index);
      end if;
   end Remove;

   procedure Upsert (Box_No : in Hash_Value; Label : in Label_String; Focal_Length : in Digit) is
      Lens_Index : Natural;
   begin
      Lens_Index := Find_Index (Box_No, Label);

      if Lens_Index > 0 then
         All_Boxes (Box_No) (Lens_Index).Focal := Focal_Length;
      else
         declare
            New_Lens : constant Lens := (Label, Focal_Length);
         begin
            All_Boxes (Box_No).Append (New_Lens);
         end;
      end if;
   end Upsert;

   function Focusing_Power return Natural is
      Total_Power : Natural := 0;
   begin
      For_Each_Box : for Box_No in Boxes'Range loop
         declare
            Box      : constant Vector := All_Boxes (Box_No);
            Position : Cursor := First (Box);
         begin
            For_Each_Lens : while Has_Element (Position) loop
               declare
                  This_Lens  : constant Lens := Element (Position);
                  This_Power : Natural;
               begin
                  This_Power  := (Box_No + 1) * To_Index (Position) * This_Lens.Focal;
                  Total_Power := @ + This_Power;
                  Position    := Next (Position);
               end;
            end loop For_Each_Lens;
         end;
      end loop For_Each_Box;

      return Total_Power;
   end Focusing_Power;

end Lens_Arrangement;