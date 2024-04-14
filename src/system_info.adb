with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body system_info is
package IO renames Ada.Text_IO;
package Serial renames GNAT.Serial_Communications;

    procedure board_info is 
        O_Size : Ada.Streams.Stream_Element_Offset := 2;
        O_Buffer : Ada.Streams.Stream_Element_Array (1..O_Size);

        I_Size : Ada.Streams.Stream_Element_Offset := 5;
        I_Buffer : Ada.Streams.Stream_Element_Array(1..I_Size);
        I_Offset : Ada.Streams.Stream_Element_Offset := 0;

        S_Port : aliased Serial.Serial_Port;

        Com_Port : Serial.Port_Name := "/dev/ttyACM0";

        Device_ID_Arr : byteArr;
        Revision_ID_Arr : byteArr;

        Device_ID : String (1..3);
        Revision_ID : String (1..4);
    begin
        --Opens the port we will communicate over and then set the specifications of the port
        S_Port.Open(Com_Port);
        S_Port.Set(Rate => Serial.B115200, Block => False, Timeout => 1000.0);

        O_Buffer(1) := Ada.Streams.Stream_Element(2);

        --set the packet to send the command code
        O_Buffer(2) := Ada.Streams.Stream_Element(info_number);

        S_Port.Write(O_Buffer);

        delay until Clock + Milliseconds(100);

        while Integer(I_Offset) < 5 loop
            S_Port.Read(I_Buffer, I_Offset);
        end loop;

        if Integer(I_Buffer(Ada.Streams.Stream_Element_Offset(5))) = 0 then
            IO.Put_Line ("Info data not received from board.");
        else
            for i in  1..2 loop
                Device_ID_Arr(i) := UInt8(I_Buffer(Ada.Streams.Stream_Element_Offset(i)));
                Revision_ID_Arr(i) := UInt8(I_Buffer(Ada.Streams.Stream_Element_Offset(i+2)));
            end loop; 

            To_Hex(To_UInt12(Device_ID_Arr), Device_ID);

            To_Hex(To_UInt16(Revision_ID_Arr), Revision_ID);

            IO.Put_Line(Ada.Characters.Latin_1.LF & "Device ID: 0x" & Device_ID);
            IO.Put_Line("Revision ID: 0x" & Revision_ID);
            IO.Put_Line("Version: 0.1.0-alpha");
        end if;
        S_Port.Close;
     
    end board_info;
 
    function description return String is
    begin
        return "Info description here";
    end description;
    -- indefinite array instead of containers
    function parameters return param_map.Map is
        params : param_map.Map;
    begin
        --params.Insert("NULL", "");

        return params;
    end parameters;
end system_info;