with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with GNAT.Serial_Communications;
with Ada.Real_Time; use Ada.Real_Time;
with utilities_cli; use utilities_cli;

package system_info is
    package AC renames Ada.Containers;
    package param_map is new AC.Indefinite_Hashed_Maps (Element_Type => String, Key_Type => String, Hash => Ada.Strings.Hash_Case_Insensitive, Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);
    function description return String;
    function parameters return param_map.Map;
    procedure board_info;
end system_info;