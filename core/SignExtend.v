module SignExtend(input [31:0] instruction,
                  output reg [31:0]output_data);

  parameter DATA_WIDTH = 32;
  parameter OPCODE_U_LUI = 7'b0110111,
   		 OPCODE_U_AUIPC = 7'b0010111,
   		 OPCODE_J_JAL = 7'b1101111,
   		 OPCODE_I_JALR = 7'b1100111,
   		 OPCODE_B_BRANCH = 7'b1100011,
   		 OPCODE_I_LOAD = 7'b0000011,  //
   		 OPCODE_S_STORE = 7'b0100011,  //
   		 OPCODE_I_IMM = 7'b0010011,
   		 OPCODE_R_ALU = 7'b0110011,  //
   		 OPCODE_I_FENCE = 7'b0001111,  //
   		 OPCODE_I_SYSTEM = 7'b1110011;
 
  parameter FUNCT3_SLL= 3'b001,
  FUNCT3_SRL_SRA = 3'b101;
  
  always @*
   begin
  case(instruction[6:0])
    OPCODE_I_IMM:begin 
      case (instruction[14:12])
      FUNCT3_SLL, FUNCT3_SRL_SRA:output_data={ {DATA_WIDTH - 5 {1'b0}}, instruction[24:20] };
      default:output_data= { {DATA_WIDTH - 12 {instruction[31]}}, instruction[31:20] };
    endcase
    end
  
 OPCODE_S_STORE :output_data={{DATA_WIDTH - 12 {instruction[31]}},  {instruction[31:25], instruction[11:7]}};
 OPCODE_B_BRANCH:output_data={{DATA_WIDTH - 13 {instruction[31]}}, {instruction[31], instruction[7], instruction[30:25], instruction[11:8]}, 1'b0  }  ; 

OPCODE_I_LOAD,OPCODE_I_JALR:output_data= { {DATA_WIDTH - 12 {instruction[31]}}, instruction[31:20] };
OPCODE_U_LUI,OPCODE_U_AUIPC:output_data={instruction[31:12], {12{1'b0}}};   
 OPCODE_J_JAL:output_data={{DATA_WIDTH-21{instruction[31]}},instruction[31],instruction[19:12],instruction[20], instruction[30:21],1'b0};
 
  endcase
   end
endmodule
