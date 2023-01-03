module riscv_programsequencer(input clock,reset
                );
  reg [31:0]a,b,mem_input;
  wire [31:0] output_pc_adder,pc,IF_instruction,x,y,z,mem_output;
  reg write_enable,we,rd;
  reg [3:0]write_transfer_i;
  
  
  reg [31:0]  IF_ID_IR , IF_ID_PC;
  reg [31:0] ID_EX_IR , ID_EX_PC , ID_EX_A,ID_EX_B,ID_EX_C,ID_EX_D,ID_EX_E;
  reg [2:0] ID_EX_type, EX_MEM_type, EX_MEM_F, MEM_WB_type;
  reg [31:0] EX_MEM_IR , EX_MEM_ALUOut ,EX_MEM_PC,EX_MEM_C,EX_MEM_E;
  wire[31:0] alu_q;
  reg EX_MEM_cond , ID_EX_SUB_SRA;
  reg [31:0] MEM_WB_IR , MEM_WB_ALUOut,MEM_addr;
  reg misaligned_flag=0;
  reg TAKEN_BRANCH=0;//////////////note him
  
  
  
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
 
 
 parameter  FUNCT3_ADD_SUB = 3'b000,
  		  FUNCT3_SLL= 3'b001,
  		  FUNCT3_SLT= 3'b010,
  		  FUNCT3_SLTU = 3'b011,
  		  FUNCT3_XOR= 3'b100,
  		  FUNCT3_SRL_SRA = 3'b101,
  		  FUNCT3_OR = 3'b110,
  		  FUNCT3_AND= 3'b111;

  parameter FUNCT3_BEQ = 3'b000,
	      FUNCT3_BNE = 3'b001,
	      FUNCT3_BLT = 3'b100,
	      FUNCT3_BGE = 3'b101,
	      FUNCT3_BLTU = 3'b110,
	      FUNCT3_BGEU = 3'b111; 

  parameter FUNCT3_LB = 	3'b000,
    		FUNCT3_LH = 	3'b001,
    		 FUNCT3_LW = 3'b010,
     		FUNCT3_LBU = 3'b100,
     		FUNCT3_LHU = 3'b101;
  
  
  parameter FUNCT3_SB = 	3'b000,
    		FUNCT3_SH = 	3'b001,
   			FUNCT3_SW = 3'b010;
  
  Adder pc_adder (
    .input_data_1(a),
    .input_data_2(b),
        .output_data(output_pc_adder)
    );

  Register pc_1 (   
        .clock(clock),
        .reset(reset),
        .new_pc(output_pc_adder),
        .pc(pc)
    );

  
  InstructionMemory instruction_memory (
    .rst_n(reset)
        .input_address(pc), 
        .output_data(IF_instruction)
    );
 

  always @(posedge clk)
    begin
    if(EX_MEM_IR[6:0]== OPCODE_B_BRANCH && EX_MEM_cond==1 )
      begin
   a=EX_MEM_PC;
   b=EX_MEM_E;
  TAKEN_BRANCH<=1;
      end
  
  else if(ID_EX_IR[6:0]==OPCODE_J_JAL)
    begin
  a=ID_EX_PC;
   b=ID_EX_E;
  TAKEN_BRANCH<=1;
    end
 
    else if(ID_EX_IR[6:0]==OPCODE_I_JALR) 
      begin
   a=ID_EX_D;
   b=ID_EX_E;
  TAKEN_BRANCH<=1;
      end
  
  else
    begin
    a=pc;
    b=4;
  end
  IF_ID_IR<=IF_instruction;
  IF_ID_PC<=pc;
    end
  ---------------------------------------------------------------------------------------------------------------

  
   
  RegisterBank register_bank (
    .clk(clock),
    .rst(reset)
    .we(write_enable),
        .ra_a(IF_ID_IR[19:15]),
        .ra_b(IF_ID_IR[24:20]),
    .wd(MEM_WB_ALUOut);
    .rda(x),
    .rdb(y)
    );
  SignExtend sign_extend (
        .instruction(IF_ID_IR), 
     .output_data(z)
    );
  alwways @(posedge clk)
  
  ID_EX_IR<=IF_ID_IR;
  ID_EX_PC<= IF_ID_PC;
  case(IF_ID_IR[6:0])
   OPCODE_R_ALU  :begin 
     ID_EX_A<=x;ID_EX_B<=y;
     ID_EX_type =  IF_ID_IR[14:12];
 	 ID_EX_SUB_SRA=IF_ID_IR[30];
   end
   OPCODE_I_IMM  :begin
     ID_EX_A<=x;ID_EX_B<=z;
     ID_EX_type =  IF_ID_IR[14:12];
 	 ID_EX_SUB_SRA=IF_ID_IR[30];
   end
   OPCODE_S_STORE:begin
     ID_EX_A<=x;ID_EX_B<=z;ID_EX_C<=y;
     ID_EX_type=0;
  	 ID_EX_SUB_SRA=0;
   end
  OPCODE_I_LOAD:begin
    ID_EX_A<=x;ID_EX_B<=z;
    ID_EX_type=0;
  	ID_EX_SUB_SRA=0;
  end
  OPCODE_B_BRANCH:begin
    ID_EX_A<=x;ID_EX_B<=y;ID_EX_E<=z;
  end
  OPCODE_U_LUI:begin
    ID_EX_A<=0;ID_EX_B<=z;
    ID_EX_type=0;
  	ID_EX_SUB_SRA=0;
  end
  OPCODE_U_AUIPC:begin
    ID_EX_A<=IF_ID_PC;ID_EX_B<=z;
    ID_EX_type=0;
  	ID_EX_SUB_SRA=0;
  end
  OPCODE_J_JAL:begin
    ID_EX_A<=IF_ID_PC;ID_EX_B<=4;ID_EX_E<=z;
    ID_EX_type=0;
   	ID_EX_SUB_SRA=0;
  end
  OPCODE_I_JALR:begin
    ID_EX_A<=IF_ID_PC;ID_EX_B<=4;ID_EX_D<=x;ID_EX_E<=z;
     ID_EX_type=0;
   	 ID_EX_SUB_SRA=0;
  end
  endcase
  end
  --------------------------------------------------------------------------------------------------------
  alu_main alu (ID_EX_A,ID_EX_B,ID_EX_type,ID_EX_SUB_SRA, alu_q,EX_MEM_F[0] , EX_MEM_F[1] , EX_MEM_F[2] );
  
  
  always @(posedge clk)
  EX_MEM_PC <=  ID_EX_PC;
    EX_MEM_IR <=  ID_EX_IR;
	EX_MEM_C <=  ID_EX_C;
  	EX_MEM_E <=  ID_EX_E;
  
  case(ID_EX_IR[6:0])
   OPCODE_R_ALU, OPCODE_I_IMM :begin
    case(ID_EX_type)
      FUNCT3_SLT:EX_MEM_ALUOut<=EX_MEM_F[1];
      FUNCT3_SLTU:EX_MEM_ALUOut<=EX_MEM_F[2];
  	  default:EX_MEM_ALUOut<=alu_q;
    endcase
  end
  
  
  OPCODE_B_BRANCH: begin

case(ID_EX_type)
	FUNCT3_BEQ : EX_MEM_cond = EX_MEM_F[0]; 
FUNCT3_BNE : EX_MEM_cond = ~ EX_MEM_F[0]; 
FUNCT3_BLT : EX_MEM_cond = EX_MEM_F[1]; 
FUNCT3_BGE : EX_MEM_cond = ~ EX_MEM_F[1]; 
FUNCT3_BLTU : EX_MEM_cond = EX_MEM_F[2]; 
FUNCT3_BGEU: EX_MEM_cond = ~ EX_MEM_F[2]; 	
endcase
end
       
default:EX_MEM_ALUOut<=alu_q;
endcase
  

  
----------------------------------------------------------------------------------------------------
  
 
  
  
  
   DataMemory data_memory (
        .clk(clock), 
         .rst_n(reset),
         .we(we), 
        .rd(rd), 
        .addr(MEM_addr), 
     .data_in(mem_input),
     .write_transfer_i(write_transfer_i),
     .data_out(mem_output)
    );

  
  always @(posedge clk)
begin

MEM_WB_IR<=EX_MEM_IR;

case(EX_MEM_IR[6:0])

OPCODE_R_ALU,OPCODE_I_IMM ,OPCODE_U_LUI,OPCODE_U_AUIPC,OPCODE_J_JAL, OPCODE_I_JALR: begin
MEM_WB_ALUOut<=EX_MEM_ALUOut;
  we=0;rd=0;
end

OPCODE_I_LOAD:begin
  MEM_addr<={EX_MEM_ALUOut[31:2],2'b0};
  rd=1;
  we=0;
  case(EX_MEM_IR[14:12])
    
    FUNCT3_LB :begin
      case(EX_MEM_ALUOut[1:0])
        2'b00:MEM_WB_ALUOut<={ {24{mem_output[7]}},   mem_output[7:0] }; 
        2'b01: MEM_WB_ALUOut<= { {24{mem_output[15]}},  mem_output[15:8] };   
        2'b10: MEM_WB_ALUOut<= { {24{mem_output[23]}},   mem_output[23:16] }; 
        2'b11: MEM_WB_ALUOut<= { {24{mem_output[31]}},   mem_output[31:24] }; 
      endcase
      end
    
    FUNCT3_LH :begin
      case(EX_MEM_ALUOut[1:0])
      2'b00: MEM_WB_ALUOut<= { {16{mem_output[15]}}, mem_output[15:0] };
      2'b10: MEM_WB_ALUOut<= { {16{mem_output[31]}}, mem_output[31:16] };
      default:misaligned_flag  = 1;
      endcase
       end
    
    FUNCT3_LW:
      MEM_WB_ALUOut<= mem_output;
      
  
    FUNCT3_LBU:begin
   case(EX_MEM_ALUOut[1:0])
                    2'b00:MEM_WB_ALUOut<= { {24{1'b0}},  mem_output[7:0] }; 
                    2'b01: MEM_WB_ALUOut<= { {24{1'b0}},mem_output[15:8]  }; 
                    2'b10: MEM_WB_ALUOut<={ {24{1'b0}}, mem_output[23:16] }; 
                    2'b11: MEM_WB_ALUOut<= { {24{1'b0}},  mem_output[31:24] }; 
   endcase
  end
  
    
  FUNCT3_LHU: begin
    case(EX_MEM_ALUOut[1:0])
                    2'b00: MEM_WB_ALUOut<={ {16{1'b0}}, mem_output[15:0] };
                    2'b10: MEM_WB_ALUOut<={ {16{1'b0}}, mem_output[31:16]};
                  default:misaligned_flag  = 1;
                endcase
            end   
    
    
  endcase     
end    


  OPCODE_S_STORE:begin
    MEM_addr<={EX_MEM_ALUOut[31:2],2'b0};
    we=1;
    rd=0;
   // if(TAKEN_BRANCH==0)begin                       /////////////////////Somebody note this line
  
     
case(EX_MEM_IR[14:12])
FUNCT3_SB:begin
  
  case(EX_MEM_ALUOut[1:0]) 
    2'b00:begin 
      mem_input <= { {24{1'b0}}, EX_MEM_C[7:0] };
      write_transfer_i={0,0,0,1};
    end
    2'b01:begin
      mem_input <={ {16{1'b0}},EX_MEM_C[7:0],  { 8{1'b0}} };
      write_transfer_i={0,0,1,0};
    end

    2'b10: begin
      mem_input <= { {8{1'b0}}, EX_MEM_C[7:0], {16{1'b0}} };
      write_transfer_i={0,1,0,0};
    end

    2'b11: begin
      mem_input <=  { EX_MEM_C[7:0], {24{1'b0}} };
      write_transfer_i={1,0,0,0};
    end
                      
     endcase
end 
  
 FUNCT3_SH :begin
   case(EX_MEM_ALUOut[1:0])
                    2'b00: begin
                       mem_input<= { {16{1'b0}}, EX_MEM_C[15:0] };
                      write_transfer_i={0,0,1,1};
                    end
                      
                    2'b10: begin
                      mem_input <= { EX_MEM_C[15:0], {16{1'b0}} };
                      write_transfer_i={1,1,0,0};
                    end
                      
                    default:misaligned_flag  = 1;
                endcase 
 end
  
  
 FUNCT3_SW: mem_input <= EX_MEM_C;
  
endcase
end

endcase
end

  
  
  -----------------------------------------------------------------------------------------------------------
  
  
  always @(posedge clk)
begin
  //if(TAKEN_BRANCH==0)                              //////note karo

case(MEM_WB_IR[6:0])

OPCODE_R_ALU,OPCODE_I_IMM ,OPCODE_U_LUI,OPCODE_U_AUIPC,OPCODE_J_JAL, OPCODE_I_JALR,OPCODE_I_LOAD:
write_enable=1;
  default:write_enable=0;
endcase
end



endmodule
  
