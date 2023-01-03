module InstructionMemory  #(parameter size = 1024)(rst_n,input_address, output_data);
  input rst_n;
  input[31:0]input_address;
  output [31:0] output_data;
  reg [7:0] memory [0 : size - 1];
  always @(negedge rst_n)
    begin
      {memory[3],memory[2],memory[1],memory[0]}<=32'd0;
    end
  assign output_data={memory[input_address + 3],
		memory[input_address + 2],
		memory[input_address + 1],
        memory[input_address + 0]};
endmodule
