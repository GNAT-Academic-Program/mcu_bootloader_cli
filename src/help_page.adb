with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body help_page is
    procedure main_page is 
        Input_Char : Character;
    begin
        Put_Line("                                                                                        Command Line Help Page                                                                                       ");
        Put_Line("The manual for command line commands for the MCU bootloader");
        Put_line("");
        Put_line("Commands:");
        Put_line("      help");
        Put_line("      info");
        Put_line("      flash");
        Put_line("      delete");
        Put_line("      quit");
        Put_line("");
        Put_line("help");
        Put_line("      displays the manual containing information on commands in the command line interface for the MCU bootloader.");
        Put_Line("");
        Put_line("      help [command]      displays the help page for the specified command.");
        Put_Line("                          example: help flash");
        Put_line("");
        Put_line("info");
        Put_line("");
        Put_line("flash");
        Put_line("");
        Put_line("delete");
        Put_line("");
        Put_line("quit");
        Put_line("      exits the command line interface.");
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