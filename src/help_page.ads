with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with utilities_cli;

package help_page is
    procedure parse_sub_command (sub_cmd_list : utilities_cli.Subcommand_Vector.Vector);
    procedure main_page;
    procedure info_page;
    procedure flash_page;
    procedure erase_page;
    procedure clear_page;
    procedure quit_page;
    procedure help_page;
    --  procedure verify_page;
end help_page;