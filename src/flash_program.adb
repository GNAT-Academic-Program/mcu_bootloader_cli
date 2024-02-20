package body flash_program is
package IO renames Ada.Text_IO;
package Serial renames GNAT.Serial_Communications;

    procedure flash_board is 
        O_Size : Ada.Streams.Stream_Element_Offset := 2;
        O_Buffer : Ada.Streams.Stream_Element_Array (1..O_Size);

        I_Size : Ada.Streams.Stream_Element_Offset := 2;
        I_Buffer : Ada.Streams.Stream_Element_Array(1..I_Size);
        I_Offset : Ada.Streams.Stream_Element_Offset;

        S_Port : aliased Serial.Serial_Port;

        Com_Port : Serial.Port_Name := "/dev/ttyACM0";
    begin
        IO.Put_Line("Flash command placeholder");

        --Opens the port we will communicate over and then set the specifications of the port
        S_Port.Open(Com_Port);
        S_Port.Set(Rate => Serial.B115200, Block => False, Timeout => 1000.0);

        --set the packet to send the first is the command code the second byte is the length
        --length represents length of package AFTER the command code every packet has a command code
        O_Buffer(1) := Ada.Streams.Stream_Element(flash_number);

        --the O_Buffer will be filled with the bytes of the program to be flashed 
        --we may have to change this function to recursive to send multiple messages
        --S_Port.Write(O_Buffer);
--
        --S_Port.Read(Buffer => I_Buffer, Last => I_Offset);
--
        --for i in 1..Integer(I_Size) loop
        --    IO.Put(I_Buffer(Ada.Streams.Stream_Element_Offset(i))'Image);
        --end loop;

    end flash_board;


    function description return String is
    begin
        return "Flash description";
    end description;
    -- indefinite array instead of containers
    function parameters return param_map.Map is
        params : param_map.Map;
    begin
        params.Insert("file", "input binary file to flash from");
        params.Insert("mode", "mode to parse the input file in");

        return params;
    end parameters;

end flash_program;