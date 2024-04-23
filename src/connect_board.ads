with GNAT.Serial_Communications; use GNAT.Serial_Communications;

package Connect_Board is
   function Connect (port : out Serial_Port; name : Port_Name) return Boolean;
   procedure Disconnect (port : in out Serial_Port);
end Connect_Board;