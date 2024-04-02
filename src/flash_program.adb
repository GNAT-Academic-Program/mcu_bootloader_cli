with utilities_cli;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body flash_program is
package IO renames Ada.Text_IO;
package Serial renames GNAT.Serial_Communications;

    procedure parse_sub_command (sub_cmd_list : utilities_cli.Subcommand_Vector.Vector) is
    begin
        if sub_cmd_list.Element(0) = "" then
            flash_handler(To_Unbounded_String(""), To_Unbounded_String(""),To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 1 then
            IO.Put_Line ("Missing arguments for the flash command. Run " & utilities_cli.bold & "help flash" & utilities_cli.unbold & " for required arguments");
        elsif sub_cmd_list.Length = 2 then
            flash_handler(sub_cmd_list.Element(0), sub_cmd_list.Element(1), To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 3 then
            flash_handler(sub_cmd_list.Element(0), sub_cmd_list.Element(1), sub_cmd_list.Element(2));
        else
            IO.Put_Line ("Too many arguments for the flash command. Run " & utilities_cli.bold & "help flash" & utilities_cli.unbold & " for required arguments");
        end if;
    end parse_sub_command;

    procedure default_flash_handler(address : in out Unbounded_String; file : in out Unbounded_String; mode : in out Unbounded_String) is
    begin
        IO.Put (Ada.Characters.Latin_1.LF & utilities_cli.bold & "Flash Program" & utilities_cli.unbold & Ada.Characters.Latin_1.LF & Ada.Characters.Latin_1.LF);
        IO.Put ("Enter the address of the flash location in hexidecimal format: ");
        address := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
        IO.Put ("Enter the file path: ");
        file := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
        IO.Put ("Enter the mode of flash (leave blank for default mode): ");
        mode := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
    end default_flash_handler;

    procedure flash_handler(address : Unbounded_String; file : Unbounded_String; mode : Unbounded_String) is 
        address_string : Unbounded_String := address;
        file_string : Unbounded_String := file;
        mode_type : Unbounded_String := mode;
    begin
        if file = "" then
            -- default flash handler
            default_flash_handler(address_string, file_string, mode_type);
        end if;
        -- flash program
        flash(address_string, file_string, mode_type);
    end flash_handler;

    procedure flash(address : Unbounded_String; file : Unbounded_String; mode : Unbounded_String) is 
        address_string : Unbounded_String := address;
        file_string : Unbounded_String := file;
        mode_type : Unbounded_String := mode;
    begin
        if mode = "" then
            -- default mode flash here, replace filler code
            IO.Put_Line("");
            IO.Put_Line("Flashing at address: ");
            Ada.Text_IO.Unbounded_IO.Put_Line (address_string);
            IO.Put_Line("with file: ");
            Ada.Text_IO.Unbounded_IO.Put_Line (file_string);
            IO.Put_Line("in mode: ");
            IO.Put_Line ("default mode");
        else -- non-default mode, lets say mode 1. Can add more mode with elseif mode = ...
            -- put flash function here, i put a filler code
            IO.Put_Line("");
            IO.Put_Line("Flashing address: ");
            Ada.Text_IO.Unbounded_IO.Put_Line (address_string);
            IO.Put_Line("with file: ");
            Ada.Text_IO.Unbounded_IO.Put_Line (file_string);
            IO.Put_Line("in mode: ");
            Ada.Text_IO.Unbounded_IO.Put_Line (mode_type);
        end if;
    end flash;


    function description return String is
    begin
        return "Flash description";
    end description;
    -- indefinite array instead of containers
    function parameters return param_map.Map is
        params : param_map.Map;
    begin
        params.Insert("address", "The address in memory to flash.");
        params.Insert("mode", "The mode to parse the input file in. The default mode is __");
        params.Insert("file", "The input binary file to flash from.");

        return params;
    end parameters;

end flash_program;