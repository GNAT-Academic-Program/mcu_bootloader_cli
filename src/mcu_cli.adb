with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--The Command_Line library lets you get the commands added after starting the program extremely easily
with Ada.Command_Line;

--these are our package imports 
with system_info;
with flash_program;
with delete_program;

procedure CLI is
   package COM renames Ada.Command_Line;
   package IO renames Ada.Text_IO;

   --enumeration of arguments
   type Arguments is (info, flash, delete);

   arg : Unbounded_String;
begin
   --When there are no arguments some information on the app such as commands and version will be displayed
   if COM.Argument_Count = 0 then
      IO.Put_Line("This will be the help page / app info");
   end if;

   --here the arguments are going to be parsed 
   for i in 1 .. COM.Argument_Count loop
      arg := To_Unbounded_String(Com.Argument(i));

      if To_String(arg)(1) /=  '-' then
         --IO.Put_Line(to_string(arg));

         --parses the main command the functions might be functions in here or we could put them in seperate packages its a placehold atm
         begin
            case Arguments'Value(To_String(arg)) is
               when info => IO.Put_Line(system_info.board_info);
               when flash => IO.Put_Line(flash_program.flash_at);
               when delete => IO.Put_Line(delete_program.delete_at);
            end case;
         
         exception
            when Constraint_Error => 
               IO.Put_Line(to_string(arg));
               IO.Put_Line("Is not a possible input");
            when others =>
               IO.Put_Line("Some error occured");
         end;
      end if;
   end loop;
end CLI;