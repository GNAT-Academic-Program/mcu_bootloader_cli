with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers.Vectors;
--with Interfaces; use Interfaces;

package utilities_cli is
    type UInt4 is mod 2**4;
    type UInt8 is mod 2**8;
    type UInt12 is mod 2**12;
    type UInt16 is mod 2**16;
    
    type subCommands is array (1..5) of Character;
    type byteArr is array (1..2) of Uint8;

    info_number : constant UInt8 := 16#07#;
    flash_number : constant UInt8 := 16#01#;
    delete_number : constant UInt8 := 16#02#;
    
    package Subcommand_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Unbounded_String);

    procedure Split_Unbounded_String (Input : Unbounded_String;
                                      Delimiter : Character_Set;
                                      Command : out Unbounded_String;
                                      Remainder : out Unbounded_String);

    function parse_subcommands (subcommands : Unbounded_String) return Subcommand_Vector.Vector;

    procedure To_Hex (Value : Uint16; Hex_String : in out String);

    procedure To_Hex (Value : Uint12; Hex_String : in out String);

    function To_Hex_Digit (Value : Uint4) return Character;

    function To_UInt16 (Val : byteArr) return UInt16; 

    function To_UInt12 (Val : byteArr) return UInt12;

    procedure Clear_Screen;
end utilities_cli;