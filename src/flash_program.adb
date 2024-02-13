with Ada.Text_IO; use Ada.Text_IO;

package body flash_program is
    procedure Test is 
    begin
        Put_Line("Flash command placeholder");
    end Test;

    function description return String is
    begin
        return "Flashes the microcontroller.";
    end description;
    -- indefinite array instead of containers
    function parameters return param_map.Map is
        params : param_map.Map;
    begin
        params.Insert("file", "input binary file to flash from");
        params.Insert("mode", "mode to parse the input file in");

        return params;
    end parameters;

end flash_program;