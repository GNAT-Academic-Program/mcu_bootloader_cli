with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with flash_program;
with Ada.Containers.Indefinite_Hashed_Maps;
with utilities_cli;
with erase_program;
with verify_program;
with system_info;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body help_page is

    procedure parse_sub_command (sub_cmd_list : utilities_cli.Subcommand_Vector.Vector) is

    begin
        for sub_cmd of sub_cmd_list loop
            if sub_cmd = "" then
                main_page;
            elsif (sub_cmd = "info" or sub_cmd = "2") then
                info_page;
            elsif (sub_cmd = "flash" or sub_cmd = "3") then
                flash_page;
            elsif (sub_cmd = "erase" or sub_cmd = "4") then
                erase_page;
            elsif (sub_cmd = "clear" or sub_cmd = "5") then
                clear_page;
            elsif (sub_cmd = "help" or sub_cmd = "1") then
                help_page;
            elsif (sub_cmd = "quit" or sub_cmd = "6") then
                quit_page;
            elsif (sub_cmd = "verify" or sub_cmd = "7") then
                verify_page;
            elsif (sub_cmd = "reset" or sub_cmd = "8") then
                reset_page;
            else
                Put_Line("Unknown sub command: " & sub_cmd & ". Please run " & utilities_cli.bold & "help" & utilities_cli.unbold & " for available commands");
            end if;
        end loop;
    end parse_sub_command;

    procedure info_page is
        Input_Char : Character;
        infoParams : system_info.param_map.Map;
        infoCursor : system_info.param_map.Cursor;
    begin
        -- title of page
        Put_line("info (2)");

        -- name
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "NAME" & utilities_cli.unbold); 
        Put_line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "info" & utilities_cli.unbold & " - displays system information");

        -- synopsis
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "SYNOPSIS" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "info" & utilities_cli.unbold & " [NULL]");

        -- description
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "DESCRIPTION" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "info" & utilities_cli.unbold & " " & system_info.description);

        -- options
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "OPTIONS" & utilities_cli.unbold);
        infoParams := system_info.parameters;
        infoCursor := system_info.param_map.First(infoParams);
        while system_info.param_map.Has_Element(infoCursor) loop
            Put_line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "[" & system_info.param_map.Key(infoCursor) & "]" & utilities_cli.unbold & "   " & system_info.param_map.Element(infoCursor));
            system_info.param_map.Next(infoCursor);
        end loop;

        -- version history
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "VERSION HISTORY" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "0.1a" & utilities_cli.unbold & " - fill here");

        -- quit command
        Put_Line(Ada.Characters.Latin_1.LF & "Press " & utilities_cli.bold & "Esc" & utilities_cli.unbold &  " to exit the help page");
        loop
            Ada.Text_IO.Get_Immediate(Input_Char);
            if Input_Char = Ada.Characters.Latin_1.ESC then
                exit;
            end if;
        end loop;
    end info_page;

    procedure flash_page is
        Input_Char : Character;
        flashParams : flash_program.param_map.Map;
        flashCursor : flash_program.param_map.Cursor;
    begin
        -- title of page
        Put_line("flash (3)");

        -- name
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "NAME" & utilities_cli.unbold); 
        Put_line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "flash" & utilities_cli.unbold & " - flashes an area in memory");

        -- synopsis
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "SYNOPSIS" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "flash" & utilities_cli.unbold & " [file][address]");

        -- description
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "DESCRIPTION" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "flash" & utilities_cli.unbold & " " & flash_program.description);

        -- options
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "OPTIONS" & utilities_cli.unbold);
        flashParams := flash_program.parameters;
        flashCursor := flash_program.param_map.First(flashParams);
        -- bugged? not in order
        while flash_program.param_map.Has_Element(flashCursor) loop
            Put_line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "[" & flash_program.param_map.Key(flashCursor) & "]" & utilities_cli.unbold & "   " & flash_program.param_map.Element(flashCursor));
            flash_program.param_map.Next(flashCursor);
        end loop;

        -- version history
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "VERSION HISTORY" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "0.1a" & utilities_cli.unbold & " - 4/10/2024 - flash implementation");
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "0.1b" & utilities_cli.unbold & " - 4/19/2024 - adding of erase and verify");

        -- quit command
        Put_Line(Ada.Characters.Latin_1.LF & "Press " & utilities_cli.bold & "Esc" & utilities_cli.unbold &  " to exit the help page");
        loop
            Ada.Text_IO.Get_Immediate(Input_Char);
            if Input_Char = Ada.Characters.Latin_1.ESC then
                exit;
            end if;
        end loop;
    end flash_page;

    procedure erase_page is
        Input_Char : Character;
        eraseParams : erase_program.param_map.Map;
        eraseCursor : erase_program.param_map.Cursor;
    begin
        -- title of page
        Put_line("erase (4)");

        -- name
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "NAME" & utilities_cli.unbold); 
        Put_line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "erase" & utilities_cli.unbold & " - erases an area in memory");

        -- synopsis
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "SYNOPSIS" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "erase" & utilities_cli.unbold & " [endSector][beginSector]");

        -- description
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "DESCRIPTION" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "erase" & utilities_cli.unbold & " " & erase_program.description);

        -- options
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "OPTIONS" & utilities_cli.unbold);
        eraseParams := erase_program.parameters;
        eraseCursor := erase_program.param_map.First(eraseParams);
        while erase_program.param_map.Has_Element(eraseCursor) loop
            Put_line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "[" & erase_program.param_map.Key(eraseCursor) & "]" & utilities_cli.unbold & "   " & erase_program.param_map.Element(eraseCursor));
            erase_program.param_map.Next(eraseCursor);
        end loop;

        -- version history
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "VERSION HISTORY" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "0.1a" & utilities_cli.unbold & " - 4/16/2024");

        -- quit command
        Put_Line(Ada.Characters.Latin_1.LF & "Press " & utilities_cli.bold & "Esc" & utilities_cli.unbold &  " to exit the help page");
        loop
            Ada.Text_IO.Get_Immediate(Input_Char);
            if Input_Char = Ada.Characters.Latin_1.ESC then
                exit;
            end if;
        end loop;
    end erase_page;

    procedure verify_page is
        Input_Char : Character;
        verifyParams : verify_program.param_map.Map;
        verifyCursor : verify_program.param_map.Cursor;
    begin
        -- title of page
        Put_line("verify (7)");

        -- name
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "NAME" & utilities_cli.unbold); 
        Put_line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "verify" & utilities_cli.unbold & " - verifies an area in memory");

        -- synopsis
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "SYNOPSIS" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "verify" & utilities_cli.unbold & " [file][address]");

        -- description
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "DESCRIPTION" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "verify" & utilities_cli.unbold & " " & verify_program.description);

        -- options
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "OPTIONS" & utilities_cli.unbold);
        verifyParams := verify_program.parameters;
        verifyCursor := verify_program.param_map.First(verifyParams);
        -- bugged? not in order
        while verify_program.param_map.Has_Element(verifyCursor) loop
            Put_line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "[" & verify_program.param_map.Key(verifyCursor) & "]" & utilities_cli.unbold & "   " & verify_program.param_map.Element(verifyCursor));
            verify_program.param_map.Next(verifyCursor);
        end loop;

        -- version history
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "VERSION HISTORY" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "0.1a" & utilities_cli.unbold & " - 4/16/2024");

        -- quit command
        Put_Line(Ada.Characters.Latin_1.LF & "Press " & utilities_cli.bold & "Esc" & utilities_cli.unbold &  " to exit the help page");
        loop
            Ada.Text_IO.Get_Immediate(Input_Char);
            if Input_Char = Ada.Characters.Latin_1.ESC then
                exit;
            end if;
        end loop;
    end verify_page;

    procedure clear_page is
        Input_Char : Character;
    begin
        -- title of page
        Put_line("clear (5)");

        -- name
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "NAME" & utilities_cli.unbold); 
        Put_line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "clear" & utilities_cli.unbold & " - clear the terminal screen");

        -- synopsis
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "SYNOPSIS" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "clear" & utilities_cli.unbold & " [NULL]");

        -- description
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "DESCRIPTION" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "clear" & utilities_cli.unbold & " clears the terminal screen by using the ANSI standard escape code '\033[2J\033[;H'.");

        -- options
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "OPTIONS" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "NULL" & utilities_cli.unbold);

        -- version history
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "VERSION HISTORY" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "0.1a" & utilities_cli.unbold & " - 3/18/2024 written by Xavier Zhang");

        -- quit command
        Put_Line(Ada.Characters.Latin_1.LF & "Press " & utilities_cli.bold & "Esc" & utilities_cli.unbold &  " to exit the help page");
        loop
            Ada.Text_IO.Get_Immediate(Input_Char);
            if Input_Char = Ada.Characters.Latin_1.ESC then
                exit;
            end if;
        end loop;
    end clear_page;

    procedure quit_page is
        Input_Char : Character;
    begin
        -- title of page
        Put_line("quit (6)");

        -- name
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "NAME" & utilities_cli.unbold); 
        Put_line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "quit" & utilities_cli.unbold & " - exits the command line interface application");

        -- synopsis
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "SYNOPSIS" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "quit" & utilities_cli.unbold & " [NULL]");

        -- description
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "DESCRIPTION" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "quit" & utilities_cli.unbold & " exits the command line interface application by calling the 'exit' command.");

        -- options
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "OPTIONS" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "NULL" & utilities_cli.unbold);

        -- version history
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "VERSION HISTORY" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "0.1a" & utilities_cli.unbold & " - 3/18/2024 written by Xavier Zhang");

        -- quit command
        Put_Line(Ada.Characters.Latin_1.LF & "Press " & utilities_cli.bold & "Esc" & utilities_cli.unbold &  " to exit the help page");
        loop
            Ada.Text_IO.Get_Immediate(Input_Char);
            if Input_Char = Ada.Characters.Latin_1.ESC then
                exit;
            end if;
        end loop;
    end quit_page;

    procedure help_page is
        Input_Char : Character;
    begin
        -- title of page
        Put_line("help (1)");

        -- name
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "NAME" & utilities_cli.unbold); 
        Put_line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "help" & utilities_cli.unbold & " - displays information of all available commands");

        -- synopsis
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "SYNOPSIS" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "help" & utilities_cli.unbold & " [commands]");

        -- description
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "DESCRIPTION" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "help" & utilities_cli.unbold & " is the command line interface application's manual page. It displays information for all available commands.");

        -- options
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "OPTIONS" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "NULL" & utilities_cli.unbold);

        -- version history
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "VERSION HISTORY" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "0.1a" & utilities_cli.unbold & " - 3/18/2024 written by Xavier Zhang");

        -- quit command
        Put_Line(Ada.Characters.Latin_1.LF & "Press " & utilities_cli.bold & "Esc" & utilities_cli.unbold &  " to exit the help page");
        loop
            Ada.Text_IO.Get_Immediate(Input_Char);
            if Input_Char = Ada.Characters.Latin_1.ESC then
                exit;
            end if;
        end loop;
    end help_page;

    procedure reset_page is
        Input_Char : Character;
    begin
        -- title of page
        Put_line("reset (8)");

        -- name
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "NAME" & utilities_cli.unbold); 
        Put_line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "reset" & utilities_cli.unbold & " - resets the microcontroller");

        -- synopsis
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "SYNOPSIS" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "reset" & utilities_cli.unbold & " [NULL]");

        -- description
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "DESCRIPTION" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "reset" & utilities_cli.unbold & " put reset description here");

        -- options
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "OPTIONS" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "NULL" & utilities_cli.unbold);

        -- version history
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "VERSION HISTORY" & utilities_cli.unbold);
        Put_Line(utilities_cli.bold & Ada.Characters.Latin_1.HT & "0.1a" & utilities_cli.unbold & " - 4/14/2024 written by Xavier Zhang");

        -- quit command
        Put_Line(Ada.Characters.Latin_1.LF & "Press " & utilities_cli.bold & "Esc" & utilities_cli.unbold &  " to exit the help page");
        loop
            Ada.Text_IO.Get_Immediate(Input_Char);
            if Input_Char = Ada.Characters.Latin_1.ESC then
                exit;
            end if;
        end loop;
    end reset_page;

    procedure main_page is 
        Input_Char : Character;
    begin
        Put_Line("                                                                                        Command Line Help Page                                                                                       ");
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "DESCRIPTION" & utilities_cli.unbold); 
        Put_Line(Ada.Characters.Latin_1.HT & "The manual for command line commands for the MCU bootloader");
        Put_line(Ada.Characters.Latin_1.LF & utilities_cli.bold & "Commands" & utilities_cli.unbold);
        Put_line(Ada.Characters.Latin_1.HT & "1. help");
        Put_line(Ada.Characters.Latin_1.HT & "2. info");
        Put_line(Ada.Characters.Latin_1.HT & "3. flash");
        Put_line(Ada.Characters.Latin_1.HT & "4. erase");
        Put_line(Ada.Characters.Latin_1.HT & "5. clear");
        Put_line(Ada.Characters.Latin_1.HT & "6. quit");
        Put_line(Ada.Characters.Latin_1.HT & "7. verify");
        Put_line(Ada.Characters.Latin_1.HT & "8. reset");

        -- quit page
        Put_Line(Ada.Characters.Latin_1.LF & "Press " & utilities_cli.bold & "Esc" & utilities_cli.unbold &  " to exit the help page");
        loop
            Ada.Text_IO.Get_Immediate(Input_Char);
            if Input_Char = Ada.Characters.Latin_1.ESC then
                exit;
            end if;
        end loop;
    end main_page;
end help_page;