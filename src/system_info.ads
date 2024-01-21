--these are complete dummy functions
with cli_types;
package system_info is
    type infoSubcommands is ('v');
    function board_info (sub_cmd : cli_types.subCommands; size : Integer) return String;
end system_info;