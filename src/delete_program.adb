package body delete_program is
    package IO renames Ada.Text_IO;
    package Serial renames GNAT.Serial_Communications;

    procedure parse_sub_command (sub_cmd_list : utilities_cli.Subcommand_Vector.Vector) is

    begin
        if sub_cmd_list.Element(0) = "" then
            flash_board(To_Unbounded_String(""),To_Unbounded_String(""));
        elsif sub_cmd_list.Length = 1 then
            flash_board(sub_cmd_list.Element(0), To_Unbounded_String("defaultmode"));
        elsif sub_cmd_list.Length = 2 then
            flash_board(sub_cmd_list.Element(0), sub_cmd_list.Element(1));
        else
            IO.Put_Line("Too many arguments for the flash command, see 'help flash' for available arguments");
        end if;
        NULL;
    end parse_sub_command;

    procedure delete_board is 

        O_Size : Ada.Streams.Stream_Element_Offset := 2;
        O_Buffer : Ada.Streams.Stream_Element_Array (1..O_Size);

        I_Size : Ada.Streams.Stream_Element_Offset := 2;
        I_Buffer : Ada.Streams.Stream_Element_Array(1..I_Size);
        I_Offset : Ada.Streams.Stream_Element_Offset;

        S_Port : aliased Serial.Serial_Port;

        Com_Port : Serial.Port_Name := "/dev/ttyACM0";
    begin
        Put_Line("Delete command placeholder");

        --Opens the port we will communicate over and then set the specifications of the port
        S_Port.Open(Com_Port);
        S_Port.Set(Rate => Serial.B115200, Block => False, Timeout => 1000.0);

        --set the packet to send the first is the command code the second byte is the length
        --length represents length of package AFTER the command code every packet has a command code
        O_Buffer(1) := Ada.Streams.Stream_Element(delete_number);

        --the O_Buffer will be filled with the bytes of the program to be flashed 
        --we may have to change this function to recursive to send multiple messages
        --S_Port.Write(O_Buffer);
--
        --S_Port.Read(Buffer => I_Buffer, Last => I_Offset);
--
        --for i in 1..Integer(I_Size) loop
        --    IO.Put(I_Buffer(Ada.Streams.Stream_Element_Offset(i))'Image);
        --end loop;
    end delete_board;

    function description return String is
    begin
        return "Delete description here";
    end description;
    -- indefinite array instead of containers
    function parameters return param_map.Map is
        params : param_map.Map;
    begin
        params.Insert("mode", "param1 description");
        params.Insert("file", "param2 description");

        return params;
    end parameters;
end delete_program;