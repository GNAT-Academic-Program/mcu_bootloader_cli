with Ada.Text_IO;
with cli_types;
package body flash_program is
    function flash_at (sub_cmd : cli_types.subCommands; size : Integer) return String is
        package IO renames Ada.Text_IO;
    begin
        for i in 1..(size-1) loop
            begin
            case flashSubcommands'Value((sub_cmd(i)'Image)) is
                when 'i' => IO.Put_Line("Internal Flash subcommand invoked");
                when 'q' => IO.Put_Line("QSPI Flash subcommand invoked");
                when 's' => IO.Put_Line("SD Card subcommand invoked");
            end case;

            Exception
                when Constraint_Error => 
                    IO.Put_Line(sub_cmd(i)'Image & " is not a recognized sub-command for 'flash' command");
                when others =>
                    IO.Put_Line("Some error occured");
            end;
        end loop;
        return "Flash Called";
    end flash_at;
end flash_program;