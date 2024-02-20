with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with flash_program;
with Ada.Containers.Indefinite_Hashed_Maps;
with utilities_cli;
with delete_program;
with system_info;

package body help_page is
    procedure main_page is 
        Input_Char : Character;
        -- make this a utilities variable in the future
        flashParams : flash_program.param_map.Map;
        flashCursor : flash_program.param_map.Cursor;
        infoParams : system_info.param_map.Map;
        infoCursor : system_info.param_map.Cursor;
        deleteParams : delete_program.param_map.Map;
        deleteCursor : delete_program.param_map.Cursor;
    begin
        Put_Line("                                                                                        Command Line Help Page                                                                                       ");
        Put_Line("The manual for command line commands for the MCU bootloader");
        Put_line("");
        Put_line("Commands:");
        Put_line("1. help");
        Put_line("2. info");
        Put_line("3. flash");
        Put_line("4. delete");
        Put_line("5. clear");
        Put_line("6. quit");
        Put_line("");
        Put_line("");
        -- help command
        Put_line("1. help");
        Put_line("");
        Put_line("Displays the manual containing information on commands in the command line interface for the MCU bootloader.");
        Put_Line("");
        Put_line("Parameters:");
        Put_Line("      add params here");
        Put_line("");
        Put_line("");
        -- info command
        Put_line("2. info");
        Put_line("");
        Put_line(system_info.description);
        Put_line("");
        Put_line("Parameters:");
        infoParams := system_info.parameters;
        infoCursor := system_info.param_map.First(infoParams);
        while system_info.param_map.Has_Element(infoCursor) loop
            Put_line("    [" & system_info.param_map.Key(infoCursor) & "]: " & system_info.param_map.Element(infoCursor));
            system_info.param_map.Next(infoCursor);
        end loop;
        Put_line("");
        Put_line("");
        -- flash command
        Put_line("3. flash");
        Put_line("");
        Put_line(flash_program.description);
        Put_line("");
        Put_line("Parameters:");
        flashParams := flash_program.parameters;
        flashCursor := flash_program.param_map.First(flashParams);
        while flash_program.param_map.Has_Element(flashCursor) loop
            Put_line("    [" & flash_program.param_map.Key(flashCursor) & "]: " & flash_program.param_map.Element(flashCursor));
            flash_program.param_map.Next(flashCursor);
        end loop;
        Put_line("");
        Put_line("");
        -- delete command
        Put_line("4. delete");
        Put_line("");
        Put_line(delete_program.description);
        Put_line("");
        Put_line("Parameters:");
        deleteParams := delete_program.parameters;
        deleteCursor := delete_program.param_map.First(deleteParams);
        while delete_program.param_map.Has_Element(deleteCursor) loop
            Put_line("    [" & delete_program.param_map.Key(deleteCursor) & "]: " & delete_program.param_map.Element(deleteCursor));
            delete_program.param_map.Next(deleteCursor);
        end loop;
        -- clear command
        Put_line("");
        Put_line("");
        Put_line("5. clear");
        Put_line("");
        Put_line("Clear command description.");
        Put_Line("");
        Put_line("Parameters:");
        Put_Line("      add params here");
        Put_line("");
        Put_line("");

        -- quit command
        Put_line("");
        Put_line("");
        Put_line("6. quit");
        Put_line("");
        Put_line("Quit command description.");
        Put_Line("");
        Put_line("Parameters:");
        Put_Line("      add params here");
        Put_line("");
        Put_line("");

        Put_Line("");
        Put_Line("Press 'Esc' to exit the help page");
        loop
            Ada.Text_IO.Get_Immediate(Input_Char);
            if Input_Char = Ada.Characters.Latin_1.ESC then
                exit;
            end if;
        end loop;
        utilities_cli.Clear_Screen;
    end main_page;
end help_page;