with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;

package body utilities_cli is
    procedure Split_Unbounded_String (Input : Unbounded_String;
                                      Delimiter : Character_Set;
                                      Command : out Unbounded_String;
                                      Remainder : out Unbounded_String) is
    Deliminter_Index_First : Natural;
    begin
        Deliminter_Index_First := Index (Input, Delimiter, 1);

        if Deliminter_Index_First = 0 then
            Command := Input;
            Remainder := To_Unbounded_String("");
        else
            Command := Unbounded_Slice (Input, 1, Deliminter_Index_First-1);

            Remainder := Unbounded_Slice (Input, Deliminter_Index_First + 1, 
                                          Length (Input));
        end if;

    end Split_Unbounded_String;

    procedure parse_subcommands (subcommands : Unbounded_String) is
        package Subcommand_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Unbounded_String);

        subcommand_list : Subcommand_Vector.Vector;
        inputcommands : Unbounded_String := subcommands;
        Command: Unbounded_String;
        Remainder : Unbounded_String;
        Delimiter : Character_Set := To_Set (' ');
        begin
            utilities_cli.Split_Unbounded_String(inputcommands, Delimiter, Command, Remainder);
            subcommand_list.Append(Command);
            inputcommands := Remainder;

            while Length(Remainder) > 0 loop
                utilities_cli.Split_Unbounded_String(inputcommands, Delimiter, Command, Remainder);
                subcommand_list.Append(Command);
                inputcommands := Remainder;
            end loop;
            -- test
            for E of subcommand_list loop
                Ada.Text_IO.Unbounded_IO.Put_Line (E);
            end loop;
    end parse_subcommands;

end utilities_cli;