with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with flash_program;
with Ada.Containers.Indefinite_Hashed_Maps;

package body help_page is
    procedure main_page is 
        Input_Char : Character;
        Params : flash_program.param_map.Map;
        Cursor : flash_program.param_map.Cursor;
    begin
        Put_Line("                                                                                        Command Line Help Page                                                                                       ");
        Put_Line("The manual for command line commands for the MCU bootloader");
        Put_line("");
        Put_line("Commands:");
        Put_line("1. help");
        Put_line("2. info");
        Put_line("3. flash");
        Put_line("4. delete");
        Put_line("5. quit");
        Put_line("");
        -- help command
        Put_line("--------");
        Put_line("| help |");
        Put_line("--------");
        Put_line("");
        Put_line("Displays the manual containing information on commands in the command line interface for the MCU bootloader.");
        Put_Line("");
        Put_line("Paramters:");
        Put_Line("      add params here");
        Put_line("");
        -- flash command
        Put_line("---------");
        Put_line("| flash |");
        Put_line("---------");
        Put_line("");
        Put_line(flash_program.description);
        Put_line("");
        Put_line("Parameters:");
        Params := flash_program.parameters;
        Cursor := flash_program.param_map.First(Params);
        while flash_program.param_map.Has_Element(Cursor) loop
            Put_line("    [" & flash_program.param_map.Key(Cursor) & "]: " & flash_program.param_map.Element(Cursor));
            flash_program.param_map.Next(Cursor);
        end loop;

        Put_Line("");
        Put_Line("Press 'Esc' to exit the help page");
        loop
            Ada.Text_IO.Get_Immediate(Input_Char);
            if Input_Char = Ada.Characters.Latin_1.ESC then
                exit;
            end if;
        end loop;
    end main_page;
end help_page;