with Ada.Text_IO;


package body system_info is
    package IO renames Ada.Text_IO;

    function board_info (sub_cmd : cli_types.subCommands; size : Integer) return String is
    begin
        for i in 1..(size-1) loop
            begin
            case infoSubcommands'Value((sub_cmd(i)'Image)) is
                when 'v' => IO.Put_Line("Verbose subcommand invoked");
            end case;

            Exception
                when Constraint_Error => 
                    IO.Put_Line(sub_cmd(i)'Image & " is not a recognized sub-command for 'info' command");
                when others =>
                    IO.Put_Line("Some error occured");
            end;
        end loop;

        
        return "Info was called";
    end board_info;

end system_info;
