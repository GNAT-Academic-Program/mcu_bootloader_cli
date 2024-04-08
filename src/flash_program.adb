with utilities_cli;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Direct_IO;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Interfaces; use Interfaces;

package body flash_program is
package IO renames Ada.Text_IO;
package Serial renames GNAT.Serial_Communications;

    procedure parse_sub_command (sub_cmd_list : utilities_cli.Subcommand_Vector.Vector) is
    begin
        if sub_cmd_list.Element(0) = "" then
            flash_handler(To_Unbounded_String(""), To_Unbounded_String(""),To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 1 then
            IO.Put_Line ("Too few arguments for the flash command. Run " & utilities_cli.bold & "help flash" & utilities_cli.unbold & " for required arguments");
        elsif sub_cmd_list.Length = 2 then
            flash_handler(sub_cmd_list.Element(0), sub_cmd_list.Element(1), To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 3 then
            flash_handler(sub_cmd_list.Element(0), sub_cmd_list.Element(1), sub_cmd_list.Element(2));
        else
            IO.Put_Line ("Too many arguments for the flash command. Run " & utilities_cli.bold & "help flash" & utilities_cli.unbold & " for required arguments");
        end if;
    end parse_sub_command;

    procedure default_flash_handler(file : in out Unbounded_String; address : in out Unbounded_String; mode : in out Unbounded_String) is
    begin
        IO.Put (Ada.Characters.Latin_1.LF & utilities_cli.bold & "Flash Program" & utilities_cli.unbold & Ada.Characters.Latin_1.LF & Ada.Characters.Latin_1.LF);
        IO.Put ("Enter the file path: ");
        file := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
        IO.Put ("Enter the address of the flash location in hexidecimal format: 0x");
        address := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
        IO.Put ("Enter the mode of flash (leave blank for default mode): ");
        mode := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);
    end default_flash_handler;

    procedure flash_handler(file : Unbounded_String; address : Unbounded_String; mode : Unbounded_String) is 
        address_string : Unbounded_String := address;
        file_string : Unbounded_String := file;
        mode_type : Unbounded_String := mode;
    begin
        if file = "" then
            -- default flash handler
            default_flash_handler(file_string, address_string, mode_type);
        end if;
        -- flash program
        flash(file_string, address_string, mode_type);
    end flash_handler;

    procedure flash(file : Unbounded_String; address : Unbounded_String; mode : Unbounded_String) is 
        address_string : String := To_String(address);
        file_string : String := To_String(file);
        mode_type : String := To_String(mode);

        source_file : Ada.Streams.Stream_IO.File_Type;

        -- Header Joe Protocol (JP):
        -- 1: total size
        -- 2: command type
        -- 3-7: address
        -- 8: length to read

        O_Size : Ada.Streams.Stream_Element_Offset := 256;
        O_Buffer : Ada.Streams.Stream_Element_Array (1..O_Size);

        I_Size : Ada.Streams.Stream_Element_Offset := 2;
        I_Buffer : Ada.Streams.Stream_Element_Array(1..I_Size);
        I_Offset : Ada.Streams.Stream_Element_Offset;

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

        File_Path : String := file_string;

        I_File : Ada.Streams.Stream_IO.File_Type;
        I_Stream : Ada.Streams.Stream_IO.Stream_Access;

        Cur_Read : Ada.Streams.Stream_Element;

        Address_Array : addrArr;
        asdf : Integer;

    begin
        if mode = "" then
        -- default mode verify here, replace filler code
                --  IO.Put (Ada.Characters.Latin_1.LF & "Flashing...");
                --  IO.Put (Ada.Characters.Latin_1.LF & "Address: ");
                --  IO.Put ("default address");
                --  IO.Put (Ada.Characters.Latin_1.LF & "File: ");
                --  Ada.Text_IO.Unbounded_IO.Put (file_string);
                --  IO.Put (Ada.Characters.Latin_1.LF & "Mode: ");
                --  IO.Put ("default mode");
        IO.Put_Line("Reading file to flash");

        --Opens the port we will communicate over and then set the specifications of the port
        S_Port.Open(Com_Port);
        S_Port.Set(Rate => Serial.B115200, Block => False, Timeout => 1000.0);

        --The second byte of the packet is the command code
        O_Buffer(2) := Ada.Streams.Stream_Element(flash_number);

        Ada.Streams.Stream_IO.Open(I_File, Ada.Streams.Stream_IO.In_File, File_Path);

        File_Size := Integer(Ada.Streams.Stream_IO.Size(I_File));

        I_Stream := Ada.Streams.Stream_IO.Stream(I_File);

        Bytes_Remaining := File_Size - Bytes_Sent;

        while Bytes_Remaining > 0 loop
            --Sets the number of bytes to send to the board in this packet

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
            delay until Clock + Milliseconds(10);

            --send the rest
            S_Port.Write(O_Buffer(2..Ada.Streams.Stream_Element_Offset(Len_To_Read)));

            Bytes_Sent := Bytes_Sent + Len_To_Read;
            Bytes_Remaining := File_Size - Bytes_Sent;

            end loop;
            Ada.Streams.Stream_IO.Close(I_File);

        else -- non-default mode, lets say mode 1. Can add more mode with elseif mode = ...
            IO.Put (Ada.Characters.Latin_1.LF & "Verifying...");
            IO.Put (Ada.Characters.Latin_1.LF & "Address: ");
            Ada.Text_IO.Unbounded_IO.Put (address);
            IO.Put (Ada.Characters.Latin_1.LF & "File: ");
            Ada.Text_IO.Unbounded_IO.Put (file);
            IO.Put (Ada.Characters.Latin_1.LF & "Mode: ");
            Ada.Text_IO.Unbounded_IO.Put (mode);
        end if;
    -- file not found
    exception
        when Ada.IO_Exceptions.Name_Error =>
            Ada.Text_IO.Put_Line ("File not found or cannot be opened.");
    end flash;


    function description return String is
    begin
        return "Flash description";
    end description;
    -- indefinite array instead of containers
    function parameters return param_map.Map is
        params : param_map.Map;
    begin
        params.Insert("address", "The address in memory to flash.");
        params.Insert("mode", "The mode to parse the input file in. The default mode is __");
        params.Insert("file", "The input binary file to flash from.");

        return params;
    end parameters;

end flash_program;