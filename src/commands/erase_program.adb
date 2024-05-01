with utilities_cli;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Direct_IO;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Interfaces; use Interfaces;

package body erase_program is
   package IO renames Ada.Text_IO;
   package Serial renames GNAT.Serial_Communications;

   procedure parse_sub_command (sub_cmd_list : utilities_cli.Subcommand_Vector.Vector) is

   begin
      if sub_cmd_list.Element (0) = "" then
         erase_handler (-1, -1);
      elsif sub_cmd_list.Length = 1 then
         erase_handler (Integer'Value (To_String (sub_cmd_list.Element(0))), Integer'Value (To_String (sub_cmd_list.Element(0))));
      elsif sub_cmd_list.Length = 2 then
         erase_handler (Integer'Value (To_String (sub_cmd_list.Element(0))), Integer'Value (To_String (sub_cmd_list.Element(1))));
      else
         IO.Put_Line ("Too many arguments for the erase command. Run " & utilities_cli.bold & "help erase" & utilities_cli.unbold & " for required arguments");
      end if;
   end parse_sub_command;

   procedure default_erase_handler (sectorStart : in out Integer; sectorEnd : in out Integer) is
   begin
      IO.Put (Ada.Characters.Latin_1.LF & utilities_cli.bold & "Erase Program" & utilities_cli.unbold & Ada.Characters.Latin_1.LF & Ada.Characters.Latin_1.LF);
      IO.Put ("Enter the starting sector to erase: ");
      sectorStart := Integer'Value (To_String (Ada.Strings.Unbounded.To_Unbounded_String (Ada.Text_IO.Get_Line)));
      IO.Put ("Enter the ending sector to erase: ");
      sectorEnd := Integer'Value (To_String (Ada.Strings.Unbounded.To_Unbounded_String (Ada.Text_IO.Get_Line)));
   end default_erase_handler;

   procedure erase_handler (sectorStart : Integer; sectorEnd : Integer) is 
      sector_start_selection : Integer := sectorStart;
      sector_end_selection : Integer := sectorEnd;
   begin
      if sector_start_selection = -1 then
         --  default erase handler
         default_erase_handler (sector_start_selection, sector_end_selection);
      end if;
      --  erase program
      erase (sector_start_selection, sector_end_selection);
   end erase_handler;

   procedure erase(sectorStart : Integer; sectorEnd : Integer) is 
      package Float_IO is new Ada.Direct_IO (Float);
      use Float_IO;

      sector_start_selection : Integer := sectorStart;
      sector_end_selection : Integer := sectorEnd;
      start_address : Integer;
      end_address : Integer;

      --  Header Joe Protocol (JP):
      --  1: total size 4
      --  2: command type 2
      --  3: sector begin 1
      --  4: sector end 1

      O_Size : Ada.Streams.Stream_Element_Offset := 4;
      O_Buffer : Ada.Streams.Stream_Element_Array (1 .. O_Size);

      I_Size : Ada.Streams.Stream_Element_Offset := 1;
      I_Buffer : Ada.Streams.Stream_Element_Array (1 .. I_Size);
      I_Offset : Ada.Streams.Stream_Element_Offset := 0;
      Clear_Buffer : Ada.Streams.Stream_Element_Array (1 .. I_Size);
      Status_Buffer : Ada.Streams.Stream_Element_Array (1 .. 1);

      S_Port : aliased Serial.Serial_Port;

      Com_Port : Serial.Port_Name := Autodetect_Port;

      --  Address we will start writing to on the board should be a parameter in the future
      Sector_Num_Start : Integer := sector_start_selection;
      Sector_Num_End : Integer := sector_end_selection;

      Sector_Number_Array : addrArr;
   begin
      --  Opens the port to communicate over, setting the serial parameters
      S_Port.Open (Com_Port);
      S_Port.Set (Rate => Serial.B115200, Block => False, Timeout => 1000.0);

      IO.Put_Line ("Erasing...");

      --  --  send ack
      --  O_Buffer(1) := Ada.Streams.Stream_Element (1);
      --  S_Port.Write(O_Buffer(1..1));
      --  size of packet
      O_Buffer (1) := Ada.Streams.Stream_Element (O_Size);

      --  The second byte of the packet is the command code
      O_Buffer (2) := Ada.Streams.Stream_Element (erase_number);
   
      --  Set the length to read in the header, 0
      O_Buffer (3) := Ada.Streams.Stream_Element (Sector_Num_Start);

      --  Set the length to read in the header, 0
      O_Buffer (4) := Ada.Streams.Stream_Element (Sector_Num_End);

      --  wait for bootloader ack for connection
      --  I_Offset := 0;
      --  while I_Offset < 1 loop
      --        S_Port.Read (I_Buffer, I_Offset);
      --  end loop;

         --  check for successful acknowledgment
      --  if Integer (I_Buffer (Ada.Streams.Stream_Element_Offset (1)))
      --              /= 1 then
      --        IO.Put_Line ("failed connection");
      --        return;
      --  end if;

      -- send the size of the packet first before the rest of the packet
      S_Port.Write (O_Buffer(1..1));

      --  checks for acknowledgement
      I_Offset := 0;
      while I_Offset < 1 loop
            S_Port.Read (Status_Buffer, I_Offset);
      end loop;

      --  check for successful acknowledgment
      if Integer (Status_Buffer (Ada.Streams.Stream_Element_Offset (1)))
                  /= 1 then 
         return;
      end if;
      Status_Buffer (Ada.Streams.Stream_Element_Offset (1)) := 0;

      -- delay so the board can allocate space
      --  delay until Clock + Milliseconds(100);

      -- send the rest
      S_Port.Write (O_Buffer(2..4));
      --  delay until Clock + Milliseconds(100);
      I_Offset := 0;
      while Integer (I_Offset) < 1 loop
         S_Port.Read(Status_Buffer, I_Offset);
      end loop;
      if Integer (Status_Buffer(Ada.Streams.Stream_Element_Offset(1)))= 1 then
         IO.Put_Line ("Erasing succeeded.");
      else
         IO.Put_Line ("Erasing failed.");
      end if;

      Status_Buffer (Ada.Streams.Stream_Element_Offset (1)) := 0;

      --  -- test
      --  S_Port.Read(I_Buffer, I_Offset);
      --  for j in 1..4 loop
      --      IO.Put (I_Buffer(Ada.Streams.Stream_Element_Offset(j))'Image);
      --  end loop;
      --  clear buffer
      I_Offset := 0;
      S_Port.Read (Clear_Buffer, I_Offset);
      I_Offset := 0;
      --  close
      S_Port.Close;
   end erase;

   function description return String is
   begin
      return "The erase command takes sectors of the internal flash and fills the sectors with all 1s.";
   end description;

   --  indefinite array instead of containers
   function parameters return param_map.Map is
      params : param_map.Map;
   begin
      params.Insert ("sectorStart", "The starting sector in memory to erase.");
      params.Insert ("sectorEnd", "The ending sector in memory to erase.");

      return params;
   end parameters;

end erase_program;