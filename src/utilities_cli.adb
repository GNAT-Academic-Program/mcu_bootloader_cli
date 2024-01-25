with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;

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

end utilities_cli;