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
            verify_handler(To_Unbounded_String(""), To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 1 then
            verify_handler(sub_cmd_list.Element(0), To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 2 then
            verify_handler(sub_cmd_list.Element(0), sub_cmd_list.Element(1));
        else
            IO.Put_Line ("Too many arguments for the verify command. Run " & utilities_cli.bold & "help verify" & utilities_cli.unbold & " for required arguments");
        end if;
    end parse_sub_command;

    procedure default_verify_handler(file : in out Unbounded_String; address : in out Unbounded_String) is
    begin
        IO.Put (Ada.Characters.Latin_1.LF & utilities_cli.bold & "Verify Program" & utilities_cli.unbold & Ada.Characters.Latin_1.LF & Ada.Characters.Latin_1.LF);
        IO.Put ("Enter the file path: ");
        file := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
        IO.Put ("Enter the address of the verify location in hexidecimal format: 0x");
        address := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
    end default_verify_handler;

    procedure verify_handler(file : Unbounded_String; address : Unbounded_String) is 
        address_string : Unbounded_String := address;
        file_string : Unbounded_String := file;
    begin
        if file = "" then
            -- default flash handler
            default_verify_handler(address_string, file_string);
        end if;
        -- flash program
        verify(address_string, file_string);
    end verify_handler;

    procedure verify(file : Unbounded_String; address : Unbounded_String) is 
        address_string : Unbounded_String := address;
        file_string : Unbounded_String := file;
    begin
        if address = "" then
        -- default mode verify here, replace filler code
            IO.Put (Ada.Characters.Latin_1.LF & "Verifying...");
            IO.Put (Ada.Characters.Latin_1.LF & "Address: ");
            IO.Put ("default address");
            IO.Put (Ada.Characters.Latin_1.LF & "File: ");
            Ada.Text_IO.Unbounded_IO.Put (file_string);
            IO.Put_Line (Ada.Characters.Latin_1.LF & "Verify failed.");
        else
            IO.Put (Ada.Characters.Latin_1.LF & "Verifying...");
            IO.Put (Ada.Characters.Latin_1.LF & "Address: ");
            Ada.Text_IO.Unbounded_IO.Put (address_string);
            IO.Put (Ada.Characters.Latin_1.LF & "File: ");
            Ada.Text_IO.Unbounded_IO.Put (file_string);
            IO.Put_Line (Ada.Characters.Latin_1.LF & "Verify failed.");
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
        params.Insert("address", "The address in memory to verify.");
        params.Insert("file", "The input binary file to verify with.");

        return params;
    end parameters;

end verify_program;