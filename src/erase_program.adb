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
        if sub_cmd_list.Element(0) = "" then
            erase_handler(-1, To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 1 then
            erase_handler(Integer'Value (To_String (sub_cmd_list.Element(0))), To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 2 then
            erase_handler(Integer'Value (To_String (sub_cmd_list.Element(0))), sub_cmd_list.Element(1));
        else
            IO.Put_Line("Too many arguments for the erase command. Run " & utilities_cli.bold & "help erase" & utilities_cli.unbold & " for required arguments");
        end if;
    end parse_sub_command;

    procedure default_erase_handler(sector : in out Integer; mode : in out Unbounded_String) is
    begin
        IO.Put (Ada.Characters.Latin_1.LF & utilities_cli.bold & "Erase Program" & utilities_cli.unbold & Ada.Characters.Latin_1.LF & Ada.Characters.Latin_1.LF);
        IO.Put ("Enter the sector to erase: ");
        sector := Integer'Value (To_String (Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line)));
        IO.Put ("Enter the mode of erase (leave blank for default mode): ");
        mode := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
    end default_erase_handler;

    procedure erase_handler(sector : Integer; mode : Unbounded_String) is 
        sector_selection : Integer := sector;
        mode_type : Unbounded_String := mode;
    begin
        if sector_selection = -1 then
            -- default erase handler
            default_erase_handler(sector_selection, mode_type);
        end if;
        -- erase program
        erase(sector_selection, mode_type);
    end erase_handler;

    procedure erase(sector : Integer; mode : Unbounded_String) is 
        package Float_IO is new Ada.Direct_IO (Float);
        use Float_IO;

        sector_selection : Integer := sector;
        mode_type : Unbounded_String := mode;
        start_address : Integer;
        end_address : Integer;

       -- Header Joe Protocol (JP):
        -- 1: total size 4
        -- 2: command type 2
        -- 3: sector begin 1
        -- 4: sector end 1

        O_Size : Ada.Streams.Stream_Element_Offset := 4;
        O_Buffer : Ada.Streams.Stream_Element_Array (1..O_Size);

        I_Size : Ada.Streams.Stream_Element_Offset := 4;
        I_Buffer : Ada.Streams.Stream_Element_Array(1..O_Size);
        I_Offset : Ada.Streams.Stream_Element_Offset;

        S_Port : aliased Serial.Serial_Port;

        Com_Port : Serial.Port_Name := "/dev/ttyACM0";

        --Address we will start writing to on the board should be a parameter in the future
        Sector_Num : Integer := sector;

        Sector_Number_Array : addrArr;
    begin

        if mode = "" then
            -- default mode
            IO.Put_Line("Erasing");

            --Opens the port we will communicate over and then set the specifications of the port
            S_Port.Open(Com_Port);
            S_Port.Set(Rate => Serial.B115200, Block => False, Timeout => 1000.0);

            -- size of packet
            O_Buffer(1) := Ada.Streams.Stream_Element(O_Size);

            --The second byte of the packet is the command code
            O_Buffer(2) := Ada.Streams.Stream_Element(erase_number);
       
            -- Set the length to read in the header, 0
            O_Buffer(3) := Ada.Streams.Stream_Element(sector);

            -- Set the length to read in the header, 0
            O_Buffer(4) := Ada.Streams.Stream_Element(sector);

            --send the size of the packet first before the rest of the packet
            S_Port.Write(O_Buffer(1..1));

            --delay so the board can allocate space
            delay until Clock + Milliseconds(100);

            --send the rest
            S_Port.Write(O_Buffer(2..4));

            --  S_Port.Read(I_Buffer, I_Offset);
            --  IO.Put (I_Buffer(Ada.Streams.Stream_Element_Offset(1))'Image);

            --  -- test
            --  S_Port.Read(I_Buffer, I_Offset);
            --  for j in 1..4 loop
            --      IO.Put (I_Buffer(Ada.Streams.Stream_Element_Offset(j))'Image);
            --  end loop;
            --close
            S_Port.Close;
        else -- non-default mode, lets say mode 1. Can add more mode with elseif mode = ...
            -- put erase function here, i put a filler code
            IO.Put (Ada.Characters.Latin_1.LF & "Erasing...");
            IO.Put (Ada.Characters.Latin_1.LF & "Sector:");
            IO.Put (sector_selection'Image);
            IO.Put (Ada.Characters.Latin_1.LF & "Starting Address:");
            IO.Put (start_address'Image);
            IO.Put (Ada.Characters.Latin_1.LF & "Ending Address:");
            IO.Put (end_address'Image);
            IO.Put (Ada.Characters.Latin_1.LF & "Mode: ");
            Ada.Text_IO.Unbounded_IO.Put (mode_type);
        end if;
    end erase;

    function description return String is
    begin
        return "Erase description here";
    end description;

    -- indefinite array instead of containers
    function parameters return param_map.Map is
        params : param_map.Map;
    begin
        params.Insert("sector", "The sector in memory to erase.");
        params.Insert("mode", "The mode to run the erase program. Optional, default mode is __");

        return params;
    end parameters;

end erase_program;