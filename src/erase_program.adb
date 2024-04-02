with utilities_cli;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body erase_program is
    package IO renames Ada.Text_IO;
    package Serial renames GNAT.Serial_Communications;

    procedure parse_sub_command (sub_cmd_list : utilities_cli.Subcommand_Vector.Vector) is

    begin
        if sub_cmd_list.Element(0) = "" then
            erase_handler(To_Unbounded_String(""), To_Unbounded_String(""), To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 1 then
            IO.Put_Line ("Missing arguments for the erase command. Run " & utilities_cli.bold & "help erase" & utilities_cli.unbold & " for required arguments");
        elsif sub_cmd_list.Length = 2 then
            erase_handler(sub_cmd_list.Element(0), sub_cmd_list.Element(1), To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 3 then
            erase_handler(sub_cmd_list.Element(0), sub_cmd_list.Element(1), sub_cmd_list.Element(2));
        else
            IO.Put_Line("Too many arguments for the erase command. Run " & utilities_cli.bold & "help erase" & utilities_cli.unbold & " for required arguments");
        end if;
    end parse_sub_command;

    procedure default_erase_handler(start_address : in out Unbounded_String; end_address : in out Unbounded_String; mode : in out Unbounded_String) is
    begin
        IO.Put (Ada.Characters.Latin_1.LF & utilities_cli.bold & "Erase Program" & utilities_cli.unbold & Ada.Characters.Latin_1.LF & Ada.Characters.Latin_1.LF);
        IO.Put ("Enter the starting address of the erase location in hexidecimal format: ");
        start_address := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
        IO.Put ("Enter the ending address of the erase location in hexidecimal format: ");
        end_address := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
        IO.Put ("Enter the mode of erase (leave blank for default mode): ");
        mode := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
    end default_erase_handler;

    procedure erase_handler(start_address : Unbounded_String; end_address : Unbounded_String; mode : Unbounded_String) is 
        start_address_string : Unbounded_String := start_address;
        end_address_string : Unbounded_String := end_address;
        mode_type : Unbounded_String := mode;
    begin
        if start_address = "" then
            -- default erase handler
            default_erase_handler(start_address_string, end_address_string, mode_type);
        end if;
        -- erase program
        erase(start_address_string, end_address_string, mode_type);
    end erase_handler;

    procedure erase(start_address : Unbounded_String; end_address : Unbounded_String; mode : Unbounded_String) is 
        start_address_string : Unbounded_String := start_address;
        end_address_string : Unbounded_String := end_address;
        mode_type : Unbounded_String := mode;
    begin
        if mode = "" then
            -- default mode erase here, replace filler code
            IO.Put (Ada.Characters.Latin_1.LF & "Erasing...");
            IO.Put (Ada.Characters.Latin_1.LF & "Starting address: ");
            Ada.Text_IO.Unbounded_IO.Put (start_address_string);
            IO.Put (Ada.Characters.Latin_1.LF & "Ending address: ");
            Ada.Text_IO.Unbounded_IO.Put (end_address_string);
            IO.Put (Ada.Characters.Latin_1.LF & "Mode: ");
            IO.Put ("default mode");
        else -- non-default mode, lets say mode 1. Can add more mode with elseif mode = ...
            -- put erase function here, i put a filler code
            IO.Put (Ada.Characters.Latin_1.LF & "Erasing...");
            IO.Put (Ada.Characters.Latin_1.LF & "Starting address: ");
            Ada.Text_IO.Unbounded_IO.Put (start_address_string);
            IO.Put (Ada.Characters.Latin_1.LF & "Ending address: ");
            Ada.Text_IO.Unbounded_IO.Put (end_address_string);
            IO.Put (Ada.Characters.Latin_1.LF & "Mode: ");
            Ada.Text_IO.Unbounded_IO.Put (mode_type);
        end if;
    end erase;

    function description return String is
    begin
        return "Erase description here";
    end description;

    -- indefinite array instead of containers
    function parameters return param_map.Map is
        params : param_map.Map;
    begin
        params.Insert("sector", "The sector in memory to erase.");
        params.Insert("size", "The size in memory to erase");
        params.Insert("mode", "The mode to run the erase program. Default mode is __");

        return params;
    end parameters;

end erase_program;