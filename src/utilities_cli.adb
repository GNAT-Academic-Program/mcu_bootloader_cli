with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;

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
         Remainder := To_Unbounded_String ("");
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
      utilities_cli.Split_Unbounded_String (inputcommands, Delimiter, Command, Remainder);
      subcommand_list.Append (Command);
      inputcommands := Remainder;

      while Length (Remainder) > 0 loop
         utilities_cli.Split_Unbounded_String (inputcommands, Delimiter, Command, Remainder);
         subcommand_list.Append (Command);
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

   function HexToInteger (Hex_String : String) return Integer is
   Result : Integer := 0;
   Digit : Character;
   Digit_Value : Integer;

   InvalidHexCharacter : exception;

   begin
      for I in Hex_String'Range loop
         Digit := Hex_String (I);

         case Digit is
            when '0'..'9' => Digit_Value := Character'Pos (Digit) - Character'Pos ('0');
            when 'A'..'F' => Digit_Value := Character'Pos (Digit) - Character'Pos ('A') + 10;
            when 'a'..'f' => Digit_Value := Character'Pos (Digit) - Character'Pos ('a') + 10;
            when others   => raise InvalidHexCharacter;
         end case;
         Result := (Result * 16) + Digit_Value;
      end loop;

      return Result;

   exception
      when InvalidHexCharacter =>
         Put_Line ("Error: Invalid hexadecimal character detected.");
         return 0;

   end HexToInteger;

   function To_Hex_Digit (Value : UInt4) return Character is
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
      Combined : UInt12;
   begin
      Combined := UInt12 (Val (1)) * 256 + UInt12 (Val (2));

      return Combined;
   end To_UInt12;

   function To_UInt16 (Val : byteArr) return UInt16 is
      Combined : UInt16;
   begin
      Combined := UInt16 (Val (1))*256 + UInt16 (Val (2));

      return Combined;
   end To_UInt16;

   --Takes a memory 32 bit memory address and converts it to 4 bytes to be sent to the board
   function Addr_To_Bytes (Val : Unsigned_32) return addrArr is
      Sol : addrArr;
   begin
      Sol (1) := UInt8 (Val / 16#1000000#);
      Sol (2) := UInt8 ( (Val / 16#10000#) and 16#FF#);
      Sol (3) := UInt8 ( (Val / 16#100#) and 16#FF#);
      Sol (4) := UInt8 (Val and 16#FF#);

      return Sol;
   end Addr_To_Bytes;

   procedure Sector_to_Addresses (sector : Integer; start_address : out Integer; end_address : out Integer) is
   begin
   -- STM32F756xx and STM32F74xxx Flash Memory Organization
   if sector = 0 then
      start_address := 16#0800_0000#;
      end_address := 16#0800_7FFF#;
   elsif sector = 1 then
      start_address := 16#0800_8000#;
      end_address := 16#0800_FFFF#;
   elsif sector = 2 then
      start_address := 16#0801_0000#;
      end_address := 16#0801_7FFF#;
   elsif sector = 3 then
      start_address := 16#0801_8000#;
      end_address := 16#0801_FFFF#;
   elsif sector = 4 then
      start_address := 16#0802_0000#;
      end_address := 16#0803_FFFF#;
   elsif sector = 5 then
      start_address := 16#0804_0000#;
      end_address := 16#0807_FFFF#;
   elsif sector = 6 then
      start_address := 16#0808_0000#;
      end_address := 16#080B_FFFF#;
   elsif sector = 7 then
      start_address := 16#080C_0000#;
      end_address := 16#080F_FFFF#;
   end if;
   end Sector_to_Addresses;

   function Addresses_to_Sector (address : Integer) return Integer is
      sector : Integer;
   begin
   -- STM32F756xx and STM32F74xxx Flash Memory Organization
   if address >= 16#0800_0000# and address <= 16#0800_7FFF# then
      sector := 0;
   elsif address >= 16#0800_8000# and address <= 16#0800_FFFF# then
      sector := 1;
   elsif address >= 16#0801_0000# and address <= 16#0801_7FFF# then
      sector := 2;
   elsif address >= 16#0801_8000# and address <= 16#0801_FFFF# then
      sector := 3;
   elsif address >= 16#0802_0000# and address <= 16#0803_FFFF# then
      sector := 4;
   elsif address >= 16#0804_0000# and address <= 16#0807_FFFF# then
      sector := 5;
   elsif address >= 16#0808_0000# and address <= 16#080B_FFFF# then
      sector := 6;
   elsif address >= 16#080C_0000# and address <= 16#080F_FFFF# then
      sector := 7;
   end if;
   return sector;
   end Addresses_to_Sector;

   procedure Progress_Bar (Pct_Completed : Float) is
      Progress_Char : constant Character := '-';
      Bar_Width : constant := 50;

      -- Procedure to print progress bar
      procedure Print_Progress_Bar (Percentage : Float) is
      begin
         Put ("[");
         for I in 1 .. Bar_Width loop
            if Float (I) / Float (Bar_Width) <= Percentage then
               Put (Progress_Char);
            else
               Put (' ');
            end if;
         end loop;
         Put ("] " & Integer (Percentage * Float (100))'Image & "%" & Ada.Characters.Latin_1.CR);
         if Percentage = 1.0 then 
            Put (Ada.Characters.Latin_1.LF);
         end if;
      end Print_Progress_Bar;

   begin
      Print_Progress_Bar (Pct_Completed);
   end Progress_Bar;

   function Autodetect_Port return Serial.Port_Name is
      Base_Path : String := "/dev/serial/by-id/";
      Dir_Search : Search_Type;
      Ser_File : Directory_Entry_Type;
   begin
      if Exists (Base_Path) then
         begin
            Start_Search (Search => Dir_Search,
                        Directory => Base_Path,
                        Pattern => "*",
                        Filter => (Directory => False, Ordinary_File => False, Special_File => True));
            if not More_Entries (Dir_Search) then
               End_Search (Dir_Search);
            else
               Get_Next_Entry (Dir_Search, Ser_File);
               End_Search (Dir_Search);
               Put_Line ("Using serial port " & Full_Name (Ser_File));
               return Serial.Port_Name (Full_Name (Ser_File));
            end if;
         exception
            --  Start_Search tries to call Modification_Time on each file it
            --  processes, which fails on a device symlink, so we instead
            --  cach the exception for the file path
            when E : Ada.Directories.Name_Error =>
               declare
                  Message : Unbounded_String := To_Unbounded_String (Exception_Message (E));
                  Ser_Path : Unbounded_String;
               begin
                  -- Get file path out from between quotes
                  Split_Unbounded_String
                    (Input     => Message,
                     Delimiter => To_Set ('"'),
                     Command   => Ser_Path,
                     Remainder => Message);
                  Split_Unbounded_String
                    (Input     => Message,
                     Delimiter => To_Set ('"'),
                     Command   => Ser_Path,
                     Remainder => Message);
                  return Serial.Port_Name (To_String (Ser_Path));
               end;
         end;
      end if;
      declare
         Line : Unbounded_String := Get_Line;
         Ser_Path : Unbounded_String;
      begin
         Split_Unbounded_String
           (Input     => Line,
            Delimiter => To_Set (' '),
            Command   => Ser_Path,
            Remainder => Line);
         return Serial.Port_Name (To_String (Ser_Path));
      end;
   end Autodetect_Port;

end utilities_cli;