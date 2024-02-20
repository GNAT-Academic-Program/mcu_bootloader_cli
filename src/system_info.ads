with cli_types;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;

package system_info is
    procedure Test;
    package AC renames Ada.Containers;
    package param_map is new AC.Indefinite_Hashed_Maps (Element_Type => String, Key_Type => String, Hash => Ada.Strings.Hash_Case_Insensitive, Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);
    --package param_map is new Ada.Containers.Indefinite_Hashed_Maps (Element_Type => String, Key_Type => String, Hash => Ada.Strings.Hash_Case_Insensitive, Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);
    function description return String;
    function parameters return param_map.Map;
end system_info;