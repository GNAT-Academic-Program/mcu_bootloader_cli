with Interfaces.C;
with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with Connect_Board; use Connect_Board;

package body Connect_Board is
   procedure Reset_STLink is
      package C renames Interfaces.C;
      use type C.int;

      function system (command : C.char_array) return C.int
      with Import, Convention => C;

      command : aliased constant C.char_array :=
         C.To_C ("st-flash reset > /dev/null 2> /dev/null");

      result : C.int;
   begin
      result := system (command);
      if result /= 0 then
         Ada.Text_IO.Put_Line ("Failed to run st-flash to reset board, " &
                               "error code " & result'Image);
      end if;
   end Reset_STLink;

   function Connect (port : out Serial_Port; name : Port_Name)
      return Boolean
   is
      Keyword : constant String := "ADBT";
      Response : String := "Ada Bootloader CLI";
   begin
      port.Open (name);
      port.Set (Rate => B115200, Block => False, Timeout => 1000.0);
      Reset_STLink;
      delay until Clock + Milliseconds (100);
      String'Write (port'Access, Keyword);
      delay until Clock + Milliseconds (200);
      String'Read (port'Access, Response);
      if Response /= "Ada Bootloader CLI" then
         Disconnect (port);
         return False;
      end if;
      return True;
   end Connect;

   procedure Disconnect (port : in out Serial_Port) is
   begin
      port.Close;
      Reset_STLink;
   end Disconnect;
end Connect_Board;