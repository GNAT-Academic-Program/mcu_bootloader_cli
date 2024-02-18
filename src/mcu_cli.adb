with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

--The Command_Line library lets you get the commands added after starting the program extremely easily
with Ada.Command_Line;

--these are our package imports 
with system_info;
with flash_program;
with delete_program;
with utilities_cli; use utilities_cli;
with help_page; use help_page;
procedure CLI is
   package COM renames Ada.Command_Line;
   package IO renames Ada.Text_IO;

   --enumeration of arguments
   type Arguments is (info, flash, delete, help, quit);
   
   arg : Unbounded_String;
   Delimiter : Character_Set;
   Command : Unbounded_String;
   Command_Arg : Arguments;
   Remainder : Unbounded_String;

   sub_cmd : subCommands;
   sub_cmd_ind : integer := 1;
begin
   Delimiter := To_Set (' ');
   loop
      -- Enter main CLI loop here
      -- arg is the string
      IO.Put("> "); 
      arg := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);

      -- change to lowercase
      -- Ada.Strings.Unbounded.Translate(arg, Ada.Strings.Maps.Constants.Lower_Case_Map);

      -- parse       
      utilities_cli.Split_Unbounded_String(arg, Delimiter, Command, Remainder);
      -- print test
      --  Ada.Text_IO.Unbounded_IO.Put_Line(Command);
      --  Ada.Text_IO.Unbounded_IO.Put_Line(Remainder);
      Ada.Strings.Unbounded.Translate(Command, Ada.Strings.Maps.Constants.Lower_Case_Map);

      begin
         Command_Arg := Arguments'Value (To_String (Command));
         case Command_Arg is
            when help => help_page.main_page;
            when info => system_info.board_info;
            when flash => flash_program.flash_board;
            when delete => delete_program.delete_board;
            when quit => exit;
         end case;
      exception
         when Constraint_Error => IO.Put_Line("command not found"); 
      end;
      IO.Put_Line("");
      IO.Put_Line("Detected sub commands: ");
      utilities_cli.parse_subcommands(Remainder);
   end loop;
end CLI;