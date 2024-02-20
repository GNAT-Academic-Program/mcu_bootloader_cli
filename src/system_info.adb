with Ada.Text_IO; use Ada.Text_IO;

package body system_info is
    procedure Test is 
    begin
        Put_Line("Info page will go here");
    end Test;

    function description return String is
    begin
        return "Info description here";
    end description;
    -- indefinite array instead of containers
    function parameters return param_map.Map is
        params : param_map.Map;
    begin
        params.Insert("param1", "param1 description");
        params.Insert("param2", "param2 description");

        return params;
    end parameters;
end system_info;