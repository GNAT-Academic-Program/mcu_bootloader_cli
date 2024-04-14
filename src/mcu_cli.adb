with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;

--The Command_Line library lets you get the commands added after starting the program extremely easily
with Ada.Command_Line;

--these are our package imports 
with system_info;
with flash_program;
with erase_program;
with verify_program;
with utilities_cli; use utilities_cli;
with help_page; use help_page;
procedure CLI is
   package COM renames Ada.Command_Line;
   package IO renames Ada.Text_IO;

   --enumeration of arguments
   type Arguments is (info, flash, erase, help, clear, quit);
   
   arg : Unbounded_String;
   Delimiter : Character_Set;
   Command : Unbounded_String;
   Command_Arg : Arguments;
   Remainder : Unbounded_String;

   sub_cmd_list : utilities_cli.Subcommand_Vector.Vector;
begin
   Delimiter := To_Set (' ');
   loop
      -- Enter main CLI loop here
      -- arg is the string
      IO.Put(utilities_cli.greenforeground & "user" & utilities_cli.resetmodes & " > "); 
      arg := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);

      -- change to lowercase
      -- Ada.Strings.Unbounded.Translate(arg, Ada.Strings.Maps.Constants.Lower_Case_Map);

      -- parse       
      utilities_cli.Split_Unbounded_String(arg, Delimiter, Command, Remainder);
      -- print test
      --  Ada.Text_IO.Unbounded_IO.Put_Line(Command);
      --  Ada.Text_IO.Unbounded_IO.Put_Line(Remainder);
      Ada.Strings.Unbounded.Translate(Command, Ada.Strings.Maps.Constants.Lower_Case_Map);
      -- sub command list
      sub_cmd_list := utilities_cli.parse_subcommands(Remainder);

      begin
         Command_Arg := Arguments'Value (To_String (Command));
         case Command_Arg is
            when help => help_page.parse_sub_command(sub_cmd_list);
            when info => system_info.board_info;
            when flash => flash_program.parse_sub_command(sub_cmd_list);
            when erase => erase_program.parse_sub_command(sub_cmd_list);
            --  when verify => verify_program.parse_sub_command(sub_cmd_list);
            when clear => utilities_cli.Clear_Screen;
            when quit => exit;
         end case;
      exception
        when Constraint_Error => IO.Put_Line("Unknown command: " & To_String (Command) & ". Please run " & utilities_cli.bold & "help" & utilities_cli.unbold & " for available commands");
      end;
      IO.Put_Line("");
      --  IO.Put_Line("Detected sub commands: ");

      --  for sub_cmd of sub_cmd_list loop
      --     Ada.Text_IO.Unbounded_IO.Put_Line (sub_cmd);
      --  end loop;
   end loop;
end CLI;