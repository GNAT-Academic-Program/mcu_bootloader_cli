with utilities_cli;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO; use Ada.Streams;
with Ada.Direct_IO;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Interfaces; use Interfaces;
with erase_program;
with verify_program;

package body flash_program is
package IO renames Ada.Text_IO;
package Serial renames GNAT.Serial_Communications;

    procedure parse_sub_command (sub_cmd_list : utilities_cli.Subcommand_Vector.Vector) is
    begin
        if sub_cmd_list.Element(0) = "" then
            flash_handler(To_Unbounded_String(""), To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 1 then
            IO.Put_Line ("Too few arguments for the flash command. Run " & utilities_cli.bold & "help flash" & utilities_cli.unbold & " for required arguments");
        elsif sub_cmd_list.Length = 2 then
            flash_handler(sub_cmd_list.Element(0), sub_cmd_list.Element(1));
        else
            IO.Put_Line ("Too many arguments for the flash command. Run " & utilities_cli.bold & "help flash" & utilities_cli.unbold & " for required arguments");
        end if;
    end parse_sub_command;

    procedure default_flash_handler(file : in out Unbounded_String; address : in out Unbounded_String) is
    begin
        IO.Put (Ada.Characters.Latin_1.LF & utilities_cli.bold & "Flash Program" & utilities_cli.unbold & Ada.Characters.Latin_1.LF & Ada.Characters.Latin_1.LF);
        IO.Put ("Enter the file path: ");
        file := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
        IO.Put ("Enter the address of the flash location in hexidecimal format: 0x");
        address := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
    end default_flash_handler;

    procedure flash_handler(file : Unbounded_String; address : Unbounded_String) is 
        address_string : Unbounded_String := address;
        file_string : Unbounded_String := file;
    begin
        if file = "" then
            -- default flash handler
            default_flash_handler(file_string, address_string);
        end if;
        -- flash program
        flash(file_string, address_string);
    end flash_handler;

    procedure flash(file : Unbounded_String; address : Unbounded_String) is 
        address_string : String := To_String(address);
        file_string : String := To_String(file);

        source_file : Ada.Streams.Stream_IO.File_Type;

        -- Header Joe Protocol (JP):
        -- 1: total size
        -- 2: command type
        -- 3-6: address
        -- 7: length to read

        O_Size : Ada.Streams.Stream_Element_Offset := 255;
        O_Buffer : Ada.Streams.Stream_Element_Array (1..O_Size);

        I_Size : Ada.Streams.Stream_Element_Offset := 1;
        I_Buffer : Ada.Streams.Stream_Element_Array(1..I_Size);
        I_Offset : Ada.Streams.Stream_Element_Offset := 0;
        counter1 : Integer := 0;

        S_Port : aliased Serial.Serial_Port;

        Com_Port : Serial.Port_Name := "/dev/ttyACM0";
        Bytes_Remaining : Integer := 0;
        Bytes_Sent : Integer := 0;
        File_Size : Integer := 0;
        Len_To_Read : Integer := 0;

        --Max number of bytes from the file sent to the board at once
        File_Chunk : Integer := 248;

        --Address we will start writing to on the board should be a parameter in the future
        Base_Mem_Address : Integer := HexToInteger(address_string);

        End_Mem_Address : Integer;

        File_Path : String := file_string;

        I_File : Ada.Streams.Stream_IO.File_Type;
        I_Stream : Ada.Streams.Stream_IO.Stream_Access;

        Cur_Read : Ada.Streams.Stream_Element;

        Address_Array : addrArr;

        Begin_Sector : Integer;
        End_Sector : Integer;

        Percentage_Complete : Float;

    begin
        -- default mode
        IO.Put_Line("Reading file...");

        --Opens the port we will communicate over and then set the specifications of the port
        S_Port.Open(Com_Port);
        S_Port.Set(Rate => Serial.B115200, Block => False, Timeout => 1000.0);

        Ada.Streams.Stream_IO.Open(I_File, Ada.Streams.Stream_IO.In_File, File_Path);

        IO.Put_Line("Reading complete." & Ada.Characters.Latin_1.LF);

        File_Size := Integer(Ada.Streams.Stream_IO.Size(I_File));

        End_Mem_Address := Base_Mem_Address + File_Size;

        Begin_Sector := utilities_cli.Addresses_to_Sector(Base_Mem_Address);
        End_Sector := utilities_cli.Addresses_to_Sector(End_Mem_Address);

        -- erase sectors to be flashed
        erase_program.erase(Begin_Sector, End_Sector);
        delay until Clock + Milliseconds(2000);

        I_Stream := Ada.Streams.Stream_IO.Stream(I_File);

        Bytes_Remaining := File_Size - Bytes_Sent;
        IO.Put_Line(Ada.Characters.Latin_1.LF & "Flashing...");

        while Bytes_Remaining > 0 loop
            --Sets the number of bytes to send to the board in this packet
            --The second byte of the packet is the command code
            O_Buffer(2) := Ada.Streams.Stream_Element(flash_number);

            if Bytes_Remaining > File_Chunk then
                Len_To_Read := File_Chunk;
            else
                Len_To_Read := Bytes_Remaining;
            end if;

            O_Buffer(1) := Ada.Streams.Stream_Element(Len_To_Read + 7);

            for i in 1..Len_To_Read loop
                Ada.Streams.Stream_Element'Read(I_Stream, Cur_Read);
                O_Buffer(Ada.Streams.Stream_Element_Offset(7+i)) := Cur_Read;
            end loop;

            -- takes the memory address we will flash to and puts it in the packet 
            Address_Array := Addr_To_Bytes(Unsigned_32(Base_Mem_Address));
            for j in 1..4 loop
                O_Buffer(Ada.Streams.Stream_Element_Offset(j+2)) := Ada.Streams.Stream_Element(Address_Array(j));

            end loop;

            O_Buffer(7) := Ada.Streams.Stream_Element(Len_To_Read);

            --send the size of the packet first before the rest of the packet
            S_Port.Write(O_Buffer(1..1));

            --delay so the board can allocate space
            delay until Clock + Milliseconds(50);

            --send the rest
            S_Port.Write(O_Buffer(2..Ada.Streams.Stream_Element_Offset(Len_To_Read + 7)));

            delay until Clock + Milliseconds(50);

            Bytes_Sent := Bytes_Sent + Len_To_Read;
            Bytes_Remaining := File_Size - Bytes_Sent;
            Base_Mem_Address := Base_Mem_Address + Len_To_Read;
            
            --counter1 := counter1 + 1;
            I_Offset := 0;
            while I_Offset < 1 loop
                S_Port.Read(I_Buffer, I_Offset);
            end loop;

            if Integer(I_Buffer(Ada.Streams.Stream_Element_Offset(1))) /= 1 then 
                exit;
            end if;
            Percentage_Complete := Float(1) - (Float(Bytes_Remaining)/Float(File_Size));
            utilities_cli.Progress_Bar(Percentage_Complete);
            delay until Clock + Milliseconds(200);
        end loop;
        Ada.Streams.Stream_IO.Close(I_File);

        --  while Integer(I_Offset) < 1 loop
        --      S_Port.Read(I_Buffer, I_Offset);
        --  end loop;
        if Integer(I_Buffer(Ada.Streams.Stream_Element_Offset(1))) = 1 then
            IO.Put_Line ("Flashing succeeded.");
            verify_program.verify(file, address);
        else
            IO.Put_Line ("Flashing failed.");
        end if;

        S_Port.Close;  

    -- file not found
    exception
        when Ada.IO_Exceptions.Name_Error =>
            Ada.Text_IO.Put_Line ("File not found or cannot be opened.");
    end flash;


    function description return String is
    begin
        return "The flash command take a binary file and memory address from the host and loads that file into that memory address and the subsequent addresses into the internal flash memory.";
    end description;
    -- indefinite array instead of containers
    function parameters return param_map.Map is
        params : param_map.Map;
    begin
        params.Insert("address", "The address in memory to flash.");
        params.Insert("file", "The input binary file to flash from.");

        return params;
    end parameters;

end flash_program;