
module control (clk);//NOTED
  input clk; 
 
  reg [31:0] PC, IF_ID_IR , IF_ID_NPC;//Doubt whether NPC is PC+4 or just PC?
  reg [31:0] ID_EX_IR , ID_EX_NPC , ID_EX_A,ID_EX_B,ID_EX_C,ID_EX_Imm;//why do we need IR in each stage ?Couldn't We just encode the instructions into 2 or 3 bits like ID_EX_ type
  reg [2:0] ID_EX_type, EX_MEM_type, EX_MEM_F, MEM_WB_type;
  reg [31:0] EX_MEM_IR , EX_MEM_ALUOut ,EX_MEM_NPC,EX_MEM_C;
  wire[31:0] alu_q;
  reg EX_MEM_cond , ID_EX_SUB_SRA;   //alu condition branch , add or sub
  reg [31:0] MEM_WB_IR , MEM_WB_ALUOut,MEM_WB_LMD,MEM_addr;//what is the need of both MEM_WB_ALUOut and MEM_WB_LMD?LMD stores loaded word  from memory ,MEM_WB_ALUOut stores the result of writeback
 reg misaligned_flag=0;
 
  reg [31:0] Reg [0:31];//reg bank separate verilog file na??
  reg [31:0] Mem [0:1023];//memory separate verilog file na??
 
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
  

  reg HALTED;//Halted isnt in risc v na??
  reg TAKEN_BRANCH;
 
  alu_main alu (ID_EX_A,ID_EX_B,ID_EX_type,ID_EX_SUB_SRA, alu_q,EX_MEM_F[0] , EX_MEM_F[1] , EX_MEM_F[2] ); 
  //-----------------------------------------------------------------------------------------------------------------------------------------------
  
  always @(posedge clk)
    begin
    if(HALTED==0)
      begin
   	 if(EX_MEM_IR[6:0]== OPCODE_B_BRANCH && EX_MEM_cond==1 )//branch
 		 begin
   		 IF_ID_IR<=Mem[EX_MEM_NPC + {{DATA_WIDTH - 13 {EX_MEM_IR[31]}}, {EX_MEM_IR[31], EX_MEM_IR[7], EX_MEM_IR[30:25], EX_MEM_IR[11:8]}, 1'b0  }];
   		 TAKEN_BRANCH<=1;
   		 IF_ID_NPC<= EX_MEM_NPC+ {{DATA_WIDTH - 13 {EX_MEM_IR[31]}}, {EX_MEM_IR[31], EX_MEM_IR[7], EX_MEM_IR[30:25], EX_MEM_IR[11:8]}, 1'b0  } +4;
   		 PC<=EX_MEM_NPC+{{DATA_WIDTH - 13 {EX_MEM_IR[31]}}, {EX_MEM_IR[31], EX_MEM_IR[7], EX_MEM_IR[30:25], EX_MEM_IR[11:8]}, 1'b0  } +4;
 		 end
   	 
else if(ID_EX_IR[6:0]==OPCODE_J_JAL)
begin
  IF_ID_IR<=Mem[ID_EX_NPC+{{DATA_WIDTH-21{ID_EX_IR[31]}},ID_EX_IR[31],ID_EX_IR[19:12],ID_EX_IR[20], ID_EX_IR[30:21],1'b0}];
IF_ID_NPC<=ID_EX_NPC+{{DATA_WIDTH-21{ID_EX_IR[31]}},ID_EX_IR[31],ID_EX_IR[19:12],ID_EX_IR[20], ID_EX_IR[30:21],1'b0}+4;
PC<=ID_EX_NPC+{{DATA_WIDTH-21{ID_EX_IR[31]}},ID_EX_IR[31],ID_EX_IR[19:12],ID_EX_IR[20], ID_EX_IR[30:21],1'b0}+4;
end

else if(ID_EX_IR[6:0]==OPCODE_I_JALR)
begin
IF_ID_IR<=Mem[Reg[ID_EX_IR[19:15]]+{ {DATA_WIDTH - 12 {ID_EX_IR[31]}}, ID_EX_IR[31:20] }];
IF_ID_NPC<=Reg[ID_EX_IR[19:15]]+{ {DATA_WIDTH - 12 {ID_EX_IR[31]}}, ID_EX_IR[31:20] }+4;
PC<=Reg[ID_EX_IR[19:15]]+{ {DATA_WIDTH - 12 {ID_EX_IR[31]}}, ID_EX_IR[31:20] }+4;
end

    else
 		 begin
   		 IF_ID_IR<=Mem[PC];
   		 IF_ID_NPC<=PC+4;
   		 PC<=PC+4;
 		 end
      end
  end
 
 
  
  
  
  //---------------------------------------------------------------------------------------------------------------------------------------
  
  
  always @(posedge clk) 
    begin
	 
    
 		 ID_EX_NPC<=IF_ID_NPC;
 		 ID_EX_IR<=IF_ID_IR;
		    
  	 
      case(IF_ID_IR[6:0])
   	 default:$display("Illegal Opcode");
  	 
   	 OPCODE_R_ALU  : begin
 		 ID_EX_type =  IF_ID_IR[14:12];
 		 ID_EX_SUB_SRA=IF_ID_IR[30];
 		 ID_EX_A<=Reg[IF_ID_IR[19:15]];
 		 ID_EX_B<=Reg[IF_ID_IR[24:20]];
   	 end
  	 
   	 OPCODE_I_IMM  : begin
 		 ID_EX_type =  IF_ID_IR[14:12];
 		 ID_EX_SUB_SRA=IF_ID_IR[30];
 		 ID_EX_A<=Reg[IF_ID_IR[19:15]];
       case (IF_ID_IR[14:12])
         FUNCT3_SLL, FUNCT3_SRL_SRA: ID_EX_B<= { {DATA_WIDTH - 5 {1'b0}}, IF_ID_IR[24:20] };
         default:ID_EX_B<= { {DATA_WIDTH - 12 {IF_ID_IR[31]}}, IF_ID_IR[31:20] };
       endcase
   	 end
  	 
  	 OPCODE_S_STORE:begin
  		 ID_EX_type=0;
  		 ID_EX_SUB_SRA=0;
  		 ID_EX_A<=Reg[IF_ID_IR[19:15]];
  		 ID_EX_B<={{DATA_WIDTH - 12 {IF_ID_IR[31]}},  {IF_ID_IR[31:25], IF_ID_IR[11:7]}};
		ID_EX_C<=Reg[IF_ID_IR[24:20]];
  	 end

  	 OPCODE_I_LOAD:begin
  		 ID_EX_type=0;
  		 ID_EX_SUB_SRA=0;
  		 ID_EX_A<=Reg[IF_ID_IR[19:15]];
  		 ID_EX_B<={ {DATA_WIDTH - 12 {IF_ID_IR[31]}}, IF_ID_IR[31:20] };

  	 end
  	 
  	 OPCODE_B_BRANCH:begin
  		 ID_EX_type=  IF_ID_IR[14:12];
  		 ID_EX_SUB_SRA=0;
  		 ID_EX_A<=Reg[IF_ID_IR[19:15]];
  		 ID_EX_B<=Reg[IF_ID_IR[24:20]];
	end

    OPCODE_U_LUI:begin
    ID_EX_type=0;
  		 ID_EX_SUB_SRA=0;
  		 ID_EX_A<=Reg[0];
  	ID_EX_B<={IF_ID_IR[31:12], {12{1'b0}}};
    
  		   	 end
    OPCODE_U_AUIPC:begin
    ID_EX_type=0;
  		 ID_EX_SUB_SRA=0;
  		 ID_EX_A<=IF_ID_NPC;
  	ID_EX_B<={IF_ID_IR[31:12], {12{1'b0}}};
   	 end
   	 
       	OPCODE_J_JAL:begin
   	 TAKEN_BRANCH<=1;
   	 ID_EX_type=0;
   	 ID_EX_SUB_SRA=0;
  		 ID_EX_A<=IF_ID_NPC;
  		 ID_EX_B<=4;

    
  		   	 end

   	  OPCODE_I_JALR:begin

   	 TAKEN_BRANCH<=1;
   	 ID_EX_type=0;
   	 ID_EX_SUB_SRA=0;
  		 ID_EX_A<=IF_ID_NPC;
  		 ID_EX_B<=4;
end
endcase
    		 
    end


  
  
  
  
  
  
  
//-------------------------------------------------------------------------------------------------------------------------------------------------  
  
  always @(posedge clk) 
   begin

	EX_MEM_NPC <=  ID_EX_NPC;
    EX_MEM_IR <=  ID_EX_IR;
	EX_MEM_C <=  ID_EX_C;
	

       // eq,lt,ltu

     case( ID_EX_IR[6:0]) 
       
  OPCODE_R_ALU, OPCODE_I_IMM :begin
    case(ID_EX_type)
      FUNCT3_SLT:EX_MEM_ALUOut<={{31{1'b0}},EX_MEM_F[1]};
      FUNCT3_SLTU:EX_MEM_ALUOut<={{31{1'b0}},EX_MEM_F[2]};
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
       
end 


  
 //--------------------------------------------------------------------------------------------------------------------------------------------- 
  
always @(posedge clk)
begin

MEM_WB_IR<=EX_MEM_IR;

case(EX_MEM_IR[6:0])

OPCODE_R_ALU,OPCODE_I_IMM ,OPCODE_U_LUI,OPCODE_U_AUIPC,OPCODE_J_JAL, OPCODE_I_JALR: 
MEM_WB_ALUOut<=EX_MEM_ALUOut;

OPCODE_I_LOAD:begin
  MEM_addr<={EX_MEM_ALUOut[31:2],2'b0};
  case(EX_MEM_IR[14:12])
    
    FUNCT3_LB :begin
      case(EX_MEM_ALUOut[1:0])
        2'b00:MEM_WB_ALUOut<={ {24{Mem[MEM_addr][7]}},   Mem[MEM_addr][7:0] }; 
        2'b01: MEM_WB_ALUOut<= { {24{Mem[MEM_addr][15]}},   Mem[MEM_addr][15:8] };   
        2'b10: MEM_WB_ALUOut<= { {24{Mem[MEM_addr][23]}},   Mem[MEM_addr][23:16] }; 
        2'b11: MEM_WB_ALUOut<= { {24{Mem[MEM_addr][31]}},   Mem[MEM_addr][31:24] }; 
      endcase
      end
    
    FUNCT3_LH :begin
      case(EX_MEM_ALUOut[1:0])
      2'b00: MEM_WB_ALUOut<= { {16{Mem[MEM_addr][15]}}, Mem[MEM_addr][15:0] };
      2'b10: MEM_WB_ALUOut<= { {16{Mem[MEM_addr][31]}}, Mem[MEM_addr][31:16] };
      default:misaligned_flag  = 1;
      endcase
       end
    
    FUNCT3_LW:
      MEM_WB_ALUOut<= Mem[MEM_addr];
      
  
    FUNCT3_LBU:begin
   case(EX_MEM_ALUOut[1:0])
                    2'b00:MEM_WB_ALUOut<= { {24{1'b0}},  Mem[MEM_addr][7:0] }; 
                    2'b01: MEM_WB_ALUOut<= { {24{1'b0}}, Mem[MEM_addr][15:8]  }; 
                    2'b10: MEM_WB_ALUOut<={ {24{1'b0}}, Mem[MEM_addr][23:16] }; 
                    2'b11: MEM_WB_ALUOut<= { {24{1'b0}},  Mem[MEM_addr][31:24] }; 
   endcase
  end
  
    
  FUNCT3_LHU: begin
    case(EX_MEM_ALUOut[1:0])
                    2'b00: MEM_WB_ALUOut<={ {16{1'b0}}, Mem[MEM_addr][15:0] };
                    2'b10: MEM_WB_ALUOut<={ {16{1'b0}}, Mem[MEM_addr][31:16]};
                  default:misaligned_flag  = 1;
                endcase
            end   
    
    
  endcase     
end    //  MEM_WB_ALUOut<=Mem[EX_MEM_ALUOut];


  OPCODE_S_STORE:begin
    if(TAKEN_BRANCH==0)begin
  
      //Mem[EX_MEM_ALUOut]<=EX_MEM_C;
case(EX_MEM_IR[14:12])
FUNCT3_SB:begin
  
  case(EX_MEM_ALUOut[1:0]) 
    2'b00: 
      Mem[MEM_addr] <= { {24{1'b0}}, EX_MEM_C[7:0] }; 


    2'b01:
      Mem[MEM_addr] <={ {16{1'b0}},EX_MEM_C[7:0],  { 8{1'b0}} };

    2'b10: 
      Mem[MEM_addr] <= { {8{1'b0}}, EX_MEM_C[7:0], {16{1'b0}} };

    2'b11: 
      Mem[MEM_addr] <=  { EX_MEM_C[7:0], {24{1'b0}} };
                      
     endcase
end 
  
 FUNCT3_SH :begin
   case(EX_MEM_ALUOut[1:0])
                    2'b00: 
                       Mem[MEM_addr] <= { {16{1'b0}}, EX_MEM_C[15:0] };
                      
                    2'b10: 
                       Mem[MEM_addr] <= { EX_MEM_C[15:0], {16{1'b0}} };
                      
                    default:misaligned_flag  = 1;
                endcase 
 end
  
  
 FUNCT3_SW: Mem[MEM_addr] <= EX_MEM_C;
  
endcase
end
end
endcase
end


//------------------------------------------------------------------------------------------------------------------------------------------------------
  
 always @(posedge clk)
begin
if(TAKEN_BRANCH==0)
begin
case(MEM_WB_IR[6:0])

OPCODE_R_ALU,OPCODE_I_IMM ,OPCODE_U_LUI,OPCODE_U_AUIPC,OPCODE_J_JAL, OPCODE_I_JALR,OPCODE_I_LOAD:
Reg[MEM_WB_IR[11:7]]<=MEM_WB_ALUOut;
endcase
end
end


endmodule
 







