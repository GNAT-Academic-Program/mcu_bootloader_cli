with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams;
with GNAT.Serial_Communications;
with Ada.Real_Time; use Ada.Real_Time;
with utilities_cli; use utilities_cli;
package flash_program is
    procedure flash_board;
end flash_program;