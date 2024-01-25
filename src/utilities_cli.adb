with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;

package body utilities_cli is
    procedure Split_Unbounded_String (String : Unbounded_String;
                                      Delimiter : Character_Set;
                                      Command : out Unbounded_String;
                                      Remainder : out Unbounded_String) is
    Index_First : Natural;
    begin
        Index_First := Index (String, Delimiter, 1);
        
        Command := Unbounded_Slice (String, 1, Index_First);

        Remainder := Unbounded_Slice (String, Index_First + 1, Length(String));

        
    end Split_Unbounded_String;

end utilities_cli;