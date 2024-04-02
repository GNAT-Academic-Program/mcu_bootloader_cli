with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams;
with GNAT.Serial_Communications;
with Ada.Real_Time; use Ada.Real_Time;
with utilities_cli; use utilities_cli;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package flash_program is
    package AC renames Ada.Containers;
    package param_map is new AC.Indefinite_Hashed_Maps (Element_Type => String, Key_Type => String, Hash => Ada.Strings.Hash_Case_Insensitive, Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);
    function description return String;
    function parameters return param_map.Map;
    procedure parse_sub_command (sub_cmd_list : utilities_cli.Subcommand_Vector.Vector);
    procedure flash_handler(address : Unbounded_String; file : Unbounded_String; mode : Unbounded_String);
    procedure default_flash_handler(address : in out Unbounded_String; file : in out Unbounded_String; mode : in out Unbounded_String);
    procedure flash(address : Unbounded_String; file : Unbounded_String; mode : Unbounded_String);
end flash_program;