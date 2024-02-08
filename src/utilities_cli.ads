with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
package utilities_cli is
    procedure Split_Unbounded_String (Input : Unbounded_String;
                                      Delimiter : Character_Set;
                                      Command : out Unbounded_String;
                                      Remainder : out Unbounded_String);

    procedure parse_subcommands (subcommands : Unbounded_String);
end utilities_cli;