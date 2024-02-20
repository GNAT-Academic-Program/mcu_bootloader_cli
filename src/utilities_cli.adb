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

    function parse_subcommands (subcommands : Unbounded_String) return Subcommand_Vector.Vector is

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
            --  for E of subcommand_list loop
            --      Ada.Text_IO.Unbounded_IO.Put_Line (E);
            --  end loop;
            return subcommand_list;
    end parse_subcommands;

    procedure Clear_Screen is
        Control_Preamble : constant Character := Character'Val (8#33#);
        Clear_Screen_Code: constant String    := "[2J";
        Home_Cursor_Code : constant String    := "[;H";

        Clear_Screen_Sequence: constant String
            := Control_Preamble & Clear_Screen_Code &
                Control_Preamble & Home_Cursor_Code;

    begin
        Put (Clear_Screen_Sequence);

    end Clear_Screen;

    function To_Hex_Digit (Value : Uint4) return Character is
    begin
       case Value is
          when 0 => return '0';
          when 1 => return '1';
          when 2 => return '2';
          when 3 => return '3';
          when 4 => return '4';
          when 5 => return '5';
          when 6 => return '6';
          when 7 => return '7';
          when 8 => return '8';
          when 9 => return '9';
          when 10 => return 'A';
          when 11 => return 'B';
          when 12 => return 'C';
          when 13 => return 'D';
          when 14 => return 'E';
          when 15 => return 'F';
       end case;
    end To_Hex_Digit;

    procedure To_Hex (Value : UInt12;
                            Hex_String : in out String) is
    begin
       Hex_String (Hex_String'First) :=
          To_Hex_Digit (UInt4 (Value / 256));
       Hex_String (Hex_String'First + 1) :=
          To_Hex_Digit (UInt4 (Value mod 256 / 16));
       Hex_String (Hex_String'First + 2) :=
          To_Hex_Digit (UInt4 (Value mod 16));
    end To_Hex;

    procedure To_Hex (Value : UInt16;
                            Hex_String : in out String) is
    begin
       Hex_String (Hex_String'First) :=
          To_Hex_Digit (UInt4 (Value / 4096));
       Hex_String (Hex_String'First + 1) :=
          To_Hex_Digit (UInt4 (Value mod 4096 / 256));
       Hex_String (Hex_String'First + 2) :=
          To_Hex_Digit (UInt4 (Value mod 256 / 16));
       Hex_String (Hex_String'First + 3) :=
          To_Hex_Digit (UInt4 (Value mod 16));
    end To_Hex;

    -- takes two bytes and converts it into a single unsigned 12 bit integer
    function To_UInt12 (Val : byteArr) return UInt12 is
        Combined : Uint12;
    begin
        Combined := Uint12(Val(1)) * 256 + UInt12(Val(2));

        return Combined;
    end To_Uint12;

    function To_UInt16 (Val : byteArr) return UInt16 is
        Combined : UInt16;
    begin
        Combined := UInt16(Val(1))*256 + UInt16(Val(2));

        return Combined;
    end To_Uint16;
end utilities_cli;