with utilities_cli;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body delete_program is
    package IO renames Ada.Text_IO;
    package Serial renames GNAT.Serial_Communications;

    procedure parse_sub_command (sub_cmd_list : utilities_cli.Subcommand_Vector.Vector) is

    begin
        if sub_cmd_list.Element(0) = "" then
            delete_handler(To_Unbounded_String(""), To_Unbounded_String(""), To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 1 then
            IO.Put_Line ("Missing arguments for the delete command. Run " & utilities_cli.bold & "help delete" & utilities_cli.unbold & " for required arguments");
        elsif sub_cmd_list.Length = 2 then
            delete_handler(sub_cmd_list.Element(0), sub_cmd_list.Element(1), To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 3 then
            delete_handler(sub_cmd_list.Element(0), sub_cmd_list.Element(1), sub_cmd_list.Element(2));
        else
            IO.Put_Line("Too many arguments for the delete command. Run " & utilities_cli.bold & "help delete" & utilities_cli.unbold & " for required arguments");
        end if;
    end parse_sub_command;

    procedure default_delete_handler(start_address : in out Unbounded_String; end_address : in out Unbounded_String; mode : in out Unbounded_String) is
    begin
        IO.Put (Ada.Characters.Latin_1.LF & utilities_cli.bold & "Delete Program" & utilities_cli.unbold & Ada.Characters.Latin_1.LF & Ada.Characters.Latin_1.LF);
        IO.Put ("Enter the starting address of the delete location in hexidecimal format: ");
        start_address := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
        IO.Put ("Enter the ending address of the delete location in hexidecimal format: ");
        end_address := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
        IO.Put ("Enter the mode of delete (leave blank for default mode): ");
        mode := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
    end default_delete_handler;

    procedure delete_handler(start_address : Unbounded_String; end_address : Unbounded_String; mode : Unbounded_String) is 
        start_address_string : Unbounded_String := start_address;
        end_address_string : Unbounded_String := end_address;
        mode_type : Unbounded_String := mode;
    begin
        if start_address = "" then
            -- default delete handler
            default_delete_handler(start_address_string, end_address_string, mode_type);
        end if;
        -- delete program
        delete(start_address_string, end_address_string, mode_type);
    end delete_handler;

    procedure delete(start_address : Unbounded_String; end_address : Unbounded_String; mode : Unbounded_String) is 
        start_address_string : Unbounded_String := start_address;
        end_address_string : Unbounded_String := end_address;
        mode_type : Unbounded_String := mode;
    begin
        if mode = "" then
            -- default mode flash here, replace filler code
            IO.Put_Line("");
            IO.Put_Line("Deleting memory with starting address: ");
            Ada.Text_IO.Unbounded_IO.Put_Line (start_address_string);
            IO.Put_Line("and ending address: ");
            Ada.Text_IO.Unbounded_IO.Put_Line (end_address_string);
            IO.Put_Line("in mode: ");
            IO.Put_Line ("default mode");
        else -- non-default mode, lets say mode 1. Can add more mode with elseif mode = ...
            -- put flash function here, i put a filler code
            IO.Put_Line("");
            IO.Put_Line("Deleting memory with starting address: ");
            Ada.Text_IO.Unbounded_IO.Put_Line (start_address_string);
            IO.Put_Line("and ending address: ");
            Ada.Text_IO.Unbounded_IO.Put_Line (end_address_string);
            IO.Put_Line("in mode: ");
            Ada.Text_IO.Unbounded_IO.Put_Line (mode_type);
        end if;
    end delete;

    function description return String is
    begin
        return "Delete description here";
    end description;

    -- indefinite array instead of containers
    function parameters return param_map.Map is
        params : param_map.Map;
    begin
        params.Insert("start address", "The start address in memory to delete.");
        params.Insert("end address", "The end address in memory to delete");
        params.Insert("mode", "The mode to run the delete program. Default mode is __");

        return params;
    end parameters;

end delete_program;