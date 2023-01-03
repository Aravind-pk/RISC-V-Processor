module alu_main
(input signed [31:0] alu_op1,alu_op2,
//input [4:0]shamt,
input[2:0]alu_func,
input sub_sra,
output reg signed [31:0] alu_q,
output reg eq,lt,ltu);
parameter ALU_OP_ADD_SUB=0,
ALU_OP_XOR = 4,
ALU_OP_OR = 6,
ALU_OP_AND = 7,
ALU_OP_SLL = 1,
ALU_OP_SR = 5;
always @*
begin
case(alu_func)
ALU_OP_ADD_SUB:begin
alu_q= sub_sra ? alu_op1-alu_op2 : alu_op1+alu_op2;
end
ALU_OP_XOR: alu_q = alu_op1 ^ alu_op2;
ALU_OP_OR: alu_q = alu_op1 | alu_op2;
ALU_OP_AND: alu_q = alu_op1 & alu_op2;
ALU_OP_SLL: alu_q = alu_op1 << alu_op2;
ALU_OP_SR:begin
alu_q= sub_sra ? alu_op1 >>> alu_op2: alu_op1 >> alu_op2;
end
default:alu_q=0;
endcase
eq = alu_op1 == alu_op2 ? 1:0;
lt = alu_op1 < alu_op2 ? 1:0;
ltu = ($unsigned(alu_op1) < $unsigned(alu_op2)) ? 1:0;
end
endmodule
