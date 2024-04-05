with utilities_cli;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Direct_IO;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body erase_program is
    package IO renames Ada.Text_IO;
    package Serial renames GNAT.Serial_Communications;

    procedure parse_sub_command (sub_cmd_list : utilities_cli.Subcommand_Vector.Vector) is

    begin
        if sub_cmd_list.Element(0) = "" then
            erase_handler(-1, To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 1 then
            erase_handler(Integer'Value (To_String (sub_cmd_list.Element(0))), To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 2 then
            erase_handler(Integer'Value (To_String (sub_cmd_list.Element(0))), sub_cmd_list.Element(1));
        else
            IO.Put_Line("Too many arguments for the erase command. Run " & utilities_cli.bold & "help erase" & utilities_cli.unbold & " for required arguments");
        end if;
    end parse_sub_command;

    procedure default_erase_handler(sector : in out Integer; mode : in out Unbounded_String) is
    begin
        IO.Put (Ada.Characters.Latin_1.LF & utilities_cli.bold & "Erase Program" & utilities_cli.unbold & Ada.Characters.Latin_1.LF & Ada.Characters.Latin_1.LF);
        IO.Put ("Enter the sector to erase: ");
        sector := Integer'Value (To_String (Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line)));
        IO.Put ("Enter the mode of erase (leave blank for default mode): ");
        mode := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
    end default_erase_handler;

    procedure erase_handler(sector : Integer; mode : Unbounded_String) is 
        sector_selection : Integer := sector;
        mode_type : Unbounded_String := mode;
    begin
        if sector_selection = -1 then
            -- default erase handler
            default_erase_handler(sector_selection, mode_type);
        end if;
        -- erase program
        erase(sector_selection, mode_type);
    end erase_handler;

    procedure erase(sector : Integer; mode : Unbounded_String) is 
        package Float_IO is new Ada.Direct_IO (Float);
        use Float_IO;



        sector_selection : Integer := sector;
        mode_type : Unbounded_String := mode;
    begin
        if mode = "" then
            -- default mode erase here, replace filler code
            IO.Put (Ada.Characters.Latin_1.LF & "Erasing...");
            IO.Put (Ada.Characters.Latin_1.LF & "Sector:");
            IO.Put (sector_selection'Image);
            IO.Put (Ada.Characters.Latin_1.LF & "Mode: ");
            IO.Put ("default mode");
        else -- non-default mode, lets say mode 1. Can add more mode with elseif mode = ...
            -- put erase function here, i put a filler code
            IO.Put (Ada.Characters.Latin_1.LF & "Erasing...");
            IO.Put (Ada.Characters.Latin_1.LF & "Sector:");
            IO.Put (sector_selection'Image);
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
        params.Insert("mode", "The mode to run the erase program. Optional, default mode is __");

        return params;
    end parameters;

end erase_program;