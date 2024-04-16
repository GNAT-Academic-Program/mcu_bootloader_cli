with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Interfaces; use Interfaces;
--with Interfaces; use Interfaces;

package utilities_cli is
    type UInt4 is mod 2**4;
    type UInt8 is mod 2**8;
    type UInt12 is mod 2**12;
    type UInt16 is mod 2**16;
    
    type subCommands is array (1..5) of Character;
    type byteArr is array (1..2) of Uint8;
    type addrArr is array (1..4) of Uint8;

    info_number : constant UInt8 := 16#07#;
    flash_number : constant UInt8 := 16#01#;
    erase_number : constant UInt8 := 16#02#;
    reset_number : constant UInt8 := 16#03#;

    -- ANSI Escape Codes
    bold : constant String := Ada.Characters.Latin_1.ESC & "[1m";
    blink : constant String := Ada.Characters.Latin_1.ESC & "[5m";
    unblink : constant String := Ada.Characters.Latin_1.ESC & "[25m";
    unbold : constant String := Ada.Characters.Latin_1.ESC & "[22m";
    resetmodes : constant String := Ada.Characters.Latin_1.ESC & "[0m";
    redforeground : constant String := Ada.Characters.Latin_1.ESC & "[31m";
    greenforeground : constant String := Ada.Characters.Latin_1.ESC & "[32m";
    
    package Subcommand_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Unbounded_String);

    procedure Split_Unbounded_String (Input : Unbounded_String;
                                      Delimiter : Character_Set;
                                      Command : out Unbounded_String;
                                      Remainder : out Unbounded_String);

    function parse_subcommands (subcommands : Unbounded_String) return Subcommand_Vector.Vector;

    procedure To_Hex (Value : Uint16; Hex_String : in out String);

    procedure To_Hex (Value : Uint12; Hex_String : in out String);

    function HexToInteger(Hex_String : String) return Integer;

    function To_Hex_Digit (Value : Uint4) return Character;

    function To_UInt16 (Val : byteArr) return UInt16; 

    function To_UInt12 (Val : byteArr) return UInt12;

    procedure Clear_Screen;

    function Addr_To_Bytes (Val : Unsigned_32) return addrArr;

    procedure Sector_to_Addresses(sector : Integer; start_address : out Integer; end_address : out Integer);

    function Addresses_to_Sector(address : Integer) return Integer;

    procedure Progress_Bar(Pct_Completed : Float);

end utilities_cli;