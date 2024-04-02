with utilities_cli;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body verify_program is
package IO renames Ada.Text_IO;
package Serial renames GNAT.Serial_Communications;

    procedure parse_sub_command (sub_cmd_list : utilities_cli.Subcommand_Vector.Vector) is
    begin
        if sub_cmd_list.Element(0) = "" then
            verify_handler(To_Unbounded_String(""), To_Unbounded_String(""), To_Unbounded_String(""), To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 1 then
            IO.Put_Line ("Missing arguments for the verify command. Run " & utilities_cli.bold & "help verify" & utilities_cli.unbold & " for required arguments");
        elsif sub_cmd_list.Length = 2 then
            IO.Put_Line ("Missing arguments for the verify command. Run " & utilities_cli.bold & "help verify" & utilities_cli.unbold & " for required arguments");
        elsif sub_cmd_list.Length = 3 then
            verify_handler(sub_cmd_list.Element(0), sub_cmd_list.Element(1), sub_cmd_list.Element(2), To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 4 then
            verify_handler(sub_cmd_list.Element(0), sub_cmd_list.Element(1), sub_cmd_list.Element(2), sub_cmd_list.Element(3));
        else
            IO.Put_Line ("Too many arguments for the verify command. Run " & utilities_cli.bold & "help verify" & utilities_cli.unbold & " for required arguments");
        end if;
    end parse_sub_command;

    procedure default_verify_handler(start_address : in out Unbounded_String; end_address : in out Unbounded_String; file : in out Unbounded_String; mode : in out Unbounded_String) is
    begin
        IO.Put (Ada.Characters.Latin_1.LF & utilities_cli.bold & "Verify Program" & utilities_cli.unbold & Ada.Characters.Latin_1.LF & Ada.Characters.Latin_1.LF);
        IO.Put ("Enter the starting address of the verify location in hexidecimal format: ");
        start_address := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
        IO.Put ("Enter the ending address of the verify location in hexidecimal format: ");
        end_address := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
        IO.Put ("Enter the file path: ");
        file := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
        IO.Put ("Enter the mode of verify (leave blank for default mode): ");
        mode := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
    end default_verify_handler;

    procedure verify_handler(start_address : Unbounded_String; end_address : Unbounded_String; file : Unbounded_String; mode : Unbounded_String) is 
        start_address_string : Unbounded_String := start_address;
        end_address_string : Unbounded_String := end_address;
        file_string : Unbounded_String := file;
        mode_type : Unbounded_String := mode;
    begin
        if file = "" then
            -- default flash handler
            default_verify_handler(start_address_string, end_address_string, file_string, mode_type);
        end if;
        -- flash program
        verify(start_address_string, end_address_string, file_string, mode_type);
    end verify_handler;

    procedure verify(start_address : Unbounded_String; end_address : Unbounded_String; file : Unbounded_String; mode : Unbounded_String) is 
        start_address_string : Unbounded_String := start_address;
        end_address_string : Unbounded_String := end_address;
        file_string : Unbounded_String := file;
        mode_type : Unbounded_String := mode;
    begin
        if mode = "" then
            -- default mode delete here, replace filler code
            IO.Put (Ada.Characters.Latin_1.LF & "Verifying...");
            IO.Put (Ada.Characters.Latin_1.LF & "Starting address: ");
            Ada.Text_IO.Unbounded_IO.Put (start_address_string);
            IO.Put (Ada.Characters.Latin_1.LF & "Ending address: ");
            Ada.Text_IO.Unbounded_IO.Put (end_address_string);
            IO.Put (Ada.Characters.Latin_1.LF & "File: ");
            Ada.Text_IO.Unbounded_IO.Put (file_string);
            IO.Put (Ada.Characters.Latin_1.LF & "Mode: ");
            IO.Put ("default mode");
        else -- non-default mode, lets say mode 1. Can add more mode with elseif mode = ...
            -- put delete function here, i put a filler code
            IO.Put (Ada.Characters.Latin_1.LF & "Verifying...");
            IO.Put (Ada.Characters.Latin_1.LF & "Starting address: ");
            Ada.Text_IO.Unbounded_IO.Put (start_address_string);
            IO.Put (Ada.Characters.Latin_1.LF & "Ending address: ");
            Ada.Text_IO.Unbounded_IO.Put (end_address_string);
            IO.Put (Ada.Characters.Latin_1.LF & "File: ");
            Ada.Text_IO.Unbounded_IO.Put (file_string);
            IO.Put (Ada.Characters.Latin_1.LF & "Mode: ");
            Ada.Text_IO.Unbounded_IO.Put (mode_type);
        end if;
    end verify;


    function description return String is
    begin
        return "Verify description";
    end description;
    -- indefinite array instead of containers
    function parameters return param_map.Map is
        params : param_map.Map;
    begin
        params.Insert("start address", "The starting address in memory to verify.");
        params.Insert("end address", "The ending address in memory to verify.");
        params.Insert("file", "The input binary file to verify with.");
        params.Insert("mode", "The mode verify. The default mode is __");

        return params;
    end parameters;

end verify_program;