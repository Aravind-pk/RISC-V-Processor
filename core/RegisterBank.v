module RegisterBank

#(parameter DATA_WIDTH = 32,
    parameter REG_DEPTH = 32,
    parameter REG_ADDR_WIDTH = 5) 
(
	rst, clk	, we,		
	ra_a	,  // Address of a
	ra_b	,  // Address of b
	wa,         //write address	
            wd ,       // write data
	rda	   		,  // data a
	rdb			   // data b
);
	
	
	// Inputs
	input rst; 
	input clk; 
	input we;  

	
	input [REG_ADDR_WIDTH-1:0]	ra_a;
	input [REG_ADDR_WIDTH-1:0]	ra_b;
	input [REG_ADDR_WIDTH-1:0]	wa;
	input [DATA_WIDTH-1:0]	wd;

	output [DATA_WIDTH-1:0]	rda;
	output [DATA_WIDTH-1:0]	rdb;
	
	// Register 
	reg [DATA_WIDTH-1:0] Reg [0:REG_DEPTH-1];



	
  assign rda = (ra_a == {REG_ADDR_WIDTH{1'b0}}) ? {DATA_WIDTH{1'b0}} : Reg[ra_a];
  assign rdb = (ra_b == {REG_ADDR_WIDTH{1'b0}}) ? {DATA_WIDTH{1'b0}} : Reg[ra_b];
	
integer j;
		
	always @ (posedge clk or negedge rst) begin 
		
		// Async Reset
		if ( !rst ) begin
			for (j=0; j < REG_DEPTH; j=j+1) begin
				Reg[j] <= {DATA_WIDTH{1'b0}}; //reset array
			end
		end 
		// Write 
		else if ( we ) begin
			Reg[wa] <= wd;
		end

	end
	
endmodule
