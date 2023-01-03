module DataMemory(input rst_n,clk,we,rd
                  input [31:0]addr,data_in,
                  input[3:0]write_transfer_i,
                  output reg [31:0]data_out);
  
  reg [31:0] dataArray[0:7];
  always @*
    begin
      if (rd)
      data_out=dataArray[addr >> 2];
    end
  integer j;
  always @ (posedge clk or negedge rst_n)
    begin
    if ( !rst_n ) begin
      for (j=0; j < 8; j=j+1) 
                dataArray[j] <= 0; //reset array
      end
        
        else if ( we ) begin
            
            if (write_transfer_i[0]) dataArray[addr >> 2][ 7: 0] <= data_in[ 7: 0];
            if (write_transfer_i[1]) dataArray[addr >> 2][15: 8] <= data_in[15: 8];
            if (write_transfer_i[2]) dataArray[addr >> 2][23:16] <= data_in[23:16];
            if (write_transfer_i[3]) dataArray[addr >> 2][31:24] <= data_in[31:24];
        end
       
    end
endmodule
