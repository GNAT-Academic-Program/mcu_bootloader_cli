with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
package utilities_cli is
    type subCommands is array (1..5) of Character;
    Type Byte is mod 2**8;
    info_number : constant Byte := 16#07#;
    flash_number : constant Byte := 16#01#;
    delete_number : constant Byte := 16#02#;
    
    procedure Split_Unbounded_String (Input : Unbounded_String;
                                      Delimiter : Character_Set;
                                      Command : out Unbounded_String;
                                      Remainder : out Unbounded_String);

    procedure parse_subcommands (subcommands : Unbounded_String);
end utilities_cli;