with utilities_cli;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Direct_IO;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Interfaces; use Interfaces;

package body reset_board is
   package IO renames Ada.Text_IO;
   package Serial renames GNAT.Serial_Communications;

   procedure reset is 
      package Float_IO is new Ada.Direct_IO (Float);
      use Float_IO;

      -- Header Joe Protocol (JP):
      -- 1: size
      -- 2: command type 
  

      O_Size : Ada.Streams.Stream_Element_Offset := 2;
      O_Buffer : Ada.Streams.Stream_Element_Array (1..O_Size);

      I_Size : Ada.Streams.Stream_Element_Offset := 1;
      I_Buffer : Ada.Streams.Stream_Element_Array(1..I_Size);
      I_Offset : Ada.Streams.Stream_Element_Offset := 0;

      S_Port : aliased Serial.Serial_Port;

      Com_Port : Serial.Port_Name := Autodetect_Port;
   begin
      IO.Put_Line("Resetting...");

      --Opens the port we will communicate over and then set the specifications of the port
      S_Port.Open(Com_Port);
      S_Port.Set(Rate => Serial.B115200, Block => False, Timeout => 1000.0);

      -- size of packet
      O_Buffer(1) := Ada.Streams.Stream_Element(O_Size);

      --The second byte of the packet is the command code
      O_Buffer(2) := Ada.Streams.Stream_Element(reset_number);

      --send the size of the packet first before the rest of the packet
      S_Port.Write(O_Buffer(1..1));

      --delay so the board can allocate space
      delay until Clock + Milliseconds(100);

      --send the rest
      S_Port.Write(O_Buffer(2..2));

      while Integer(I_Offset) < 1 loop
         S_Port.Read(I_Buffer, I_Offset);
      end loop;
      if Integer(I_Buffer(Ada.Streams.Stream_Element_Offset(1)))= 1 then
         IO.Put_Line ("Resetting succeeded.");
      else
         IO.Put_Line ("Resetting failed.");
      end if;

      --close
      S_Port.Close;
   end reset;

   function description return String is
   begin
      return "Resets the microcontroller";
   end description;

   -- indefinite array instead of containers
   function parameters return param_map.Map is
      params : param_map.Map;
   begin
      params.Insert("", "");


      return params;
   end parameters;

end reset_board;