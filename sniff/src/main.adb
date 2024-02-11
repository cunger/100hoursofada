with Port_Scanner;
with Ada.Command_Line;

procedure Main is
   package CLI renames Ada.Command_Line;

   Host_Name : constant String := (
      if CLI.Argument_Count > 0 then CLI.Argument (1) else "localhost"
   );
begin

   Port_Scanner.Sniff (Host_Name);

end Main;
