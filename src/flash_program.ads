--these are complete dummy functions 
with cli_types;
package flash_program is
    type flashSubCommands is ('i', 'q', 's');
    function flash_at (sub_cmd : cli_types.subCommands; size : Integer) return String;
end flash_program;