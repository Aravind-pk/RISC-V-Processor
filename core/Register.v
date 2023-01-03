module Register #(parameter n = 32) (clock, reset, new_pc, pc);

	input clock;
   input reset;
  input [n - 1 : 0] new_pc;
  output reg [n - 1 : 0] pc;

  always @ (posedge clock or negedge reset) begin
		
    if(~reset)
			pc <= 0;
		else
			pc<= new_pc;
	end

endmodule
