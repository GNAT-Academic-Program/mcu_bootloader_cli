with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers.Vectors;

package utilities_cli is
    type subCommands is array (1..5) of Character;
    Type Byte is mod 2**8;
    info_number : constant Byte := 16#07#;
    flash_number : constant Byte := 16#01#;
    delete_number : constant Byte := 16#02#;
    
    package Subcommand_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Unbounded_String);

    procedure Split_Unbounded_String (Input : Unbounded_String;
                                      Delimiter : Character_Set;
                                      Command : out Unbounded_String;
                                      Remainder : out Unbounded_String);

    function parse_subcommands (subcommands : Unbounded_String) return Subcommand_Vector.Vector;

    procedure Clear_Screen;
end utilities_cli;