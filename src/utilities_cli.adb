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

    function HexToInteger(Hex_String : String) return Integer is
    Result : Integer := 0;
    Digit : Character;
    Digit_Value : Integer;

    InvalidHexCharacter : exception;

    begin
        for I in Hex_String'Range loop
            Digit := Hex_String(I);

            case Digit is
                when '0'..'9' => Digit_Value := Character'Pos(Digit) - Character'Pos('0');
                when 'A'..'F' => Digit_Value := Character'Pos(Digit) - Character'Pos('A') + 10;
                when 'a'..'f' => Digit_Value := Character'Pos(Digit) - Character'Pos('a') + 10;
                when others   => raise InvalidHexCharacter;
            end case;
            Result := (Result * 16) + Digit_Value;
        end loop;

        return Result;

    exception
        when InvalidHexCharacter =>
            Put_Line("Error: Invalid hexadecimal character detected.");
            return 0;

    end HexToInteger;

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

    --Takes a memory 32 bit memory address and converts it to 4 bytes to be sent to the board
    function Addr_To_Bytes (Val : Unsigned_32) return addrArr is
        Sol : addrArr;
    begin
        Sol(1) := UInt8(Shift_Right(Val, 24));
        Sol(2) := UInt8(Shift_Right((Val mod 16#FF000000#), 16));
        Sol(3) := UInt8(Shift_Right((Val mod 16#FFFF0000#), 8));
        Sol(4) := UInt8(Val mod 16#FFFFFF00#);

        return Sol;
    end Addr_To_Bytes;

    procedure Sector_to_Addresses(sector : Integer; start_address : out Integer; end_address : out Integer) is
    begin
    if sector = 0 then
        start_address := 134217728;
        end_address := 134250495;
    elsif sector = 1 then
        start_address := 134250496;
        end_address := 134283263;
    elsif sector = 2 then
        start_address := 134283264;
        end_address := 134316031;
    elsif sector = 3 then
        start_address := 134316032;
        end_address := 134348799;
    elsif sector = 4 then
        start_address := 134348800;
        end_address := 134479871;
    elsif sector = 5 then
        start_address := 134479872;
        end_address := 134742015;
    elsif sector = 6 then
        start_address := 134742016;
        end_address := 135004159;
    elsif sector = 7 then
        start_address := 135004160;
        end_address := 135266303;
    else
        raise Program_Error with "Invalid sector number: " & sector'Image;
    end if;
    exception
        when Program_Error =>
            Put_Line("Invalid sector number");
    end Sector_to_Addresses;

end utilities_cli;