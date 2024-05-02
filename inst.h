#pragma once

#include <stdio.h>

#include "symbol.h"
#include "translate.h"

typedef int I_Label;
typedef int I_Temp;
typedef struct I_Reg_ * I_Reg;
typedef struct I_Mem_ * I_Mem;
typedef struct I_String_ * I_String;
typedef int I_StringLabel;
typedef struct I_StringList_ * I_StringList;
typedef struct I_Operand_ * I_Operand;
typedef struct I_Inst_ * I_Inst;
typedef struct I_InstList_ * I_InstList;
typedef struct I_LoopEndList_ * I_LoopEndList;
typedef struct I_Function_ * I_Function;
typedef struct I_FunctionList_ * I_FunctionList;

typedef enum {
    RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP,
    R8, R9, R10, R11, R12, R13, R14, R15, RNONE
} I_NamedReg;

struct I_Reg_ {
    enum { I_NAMED, I_TEMP } kind;
    union {
        I_NamedReg reg;
        I_Temp temp;
    } u;
};

struct I_Mem_ {
    S_Symbol name;
    I_Reg base;
    I_Reg index;
    int scale;
    int offset;
};

struct I_String_ {
    string str;
    I_StringLabel label;
};

struct I_StringList_ {
    I_String head;
    I_StringList tail;
    I_StringList end;
};

typedef enum {
    I_BINARY,
    I_LOAD,
    I_STORE,
    I_ADD,
    I_SUB,
    I_MUL,
    I_CMP,
    I_TEST,
    I_AND,
    I_OR,
    I_UNARY,
    I_DIV,
    I_PCALL,
    I_FCALL,
    I_JMP,
    I_JE,
    I_JNE,
    I_JLT,
    I_JLE,
    I_JGE,
    I_JGT,
    I_JNZ,
    I_JZ,
    I_PUSH,
    I_POP,
    I_LABEL,
    I_NULLARY,
    I_EXTEND,
    I_RET
} I_InstClass;

struct I_Operand_ {
    enum {
        I_IMM_OPRND,
        I_REG_OPRND,
        I_MEM_OPRND,
        I_STR_LABEL_OPRND,
        I_LABEL_OPRND,
        I_TARGET_OPRND
    } kind;
    union {
        int imm;
        I_Reg reg;
        I_Mem mem;
        I_StringLabel string_label;
        I_Label label;
        string target;
    } u;
};

struct I_Inst_ {
    I_InstClass class;
    int size;
    int seq;
    I_Operand src;
    I_Operand dst;
};

struct I_InstList_ {
    I_Inst head;
    I_InstList tail;
    I_InstList end;
};

struct I_LoopEndList_ {
    I_Operand head;
    I_LoopEndList tail;
};

struct I_Function_ {
    S_Symbol name;
    int frame_size;
    int nesting_level;
    I_StringList strings;
    I_InstList instructions;
    I_LoopEndList loop_end_labels;
    I_Temp temp;
    int aligned_frame_size;
    int effective_frame_size;
    int padding;
};

struct I_FunctionList_ {
    I_Function head;
    I_FunctionList tail;
};

extern const int I_num_callee_saved_regs;

I_Reg make_I_NamedReg(I_NamedReg reg);
I_Reg make_I_TempReg(I_Temp temp);
I_Mem make_I_MemFromIR(TR_Exp mem_exp, I_Reg named_base);
I_Mem make_I_DerefMem(I_Reg named_base);
I_Mem make_I_IndexedMem(I_Reg base_reg, I_Reg index_temp, int scale);
I_Mem make_I_OffsetMem(I_Reg base_reg, int offset);
I_String make_I_String(string str, I_StringLabel label);
I_StringList make_I_StringList(I_String str);
void I_add_string(I_Function func, I_String str);
I_Operand make_I_ImmOperand(int num);
I_Operand make_I_RegOperand(I_Reg reg);
I_Operand make_I_MemOperand(I_Mem mem);
I_Operand make_I_StringLabelOperand(I_StringLabel string_label);
I_Operand make_I_LabelOperand(I_Label label);
I_Operand I_trans_exp(I_Function func, TR_Exp exp);
void I_trans_stm(I_Function func, TR_Stm stm);
I_Inst make_I_Inst(I_InstClass class, int size);
I_InstList make_I_InstList(I_Inst inst);
void I_add_inst(I_Function func, I_Inst inst);
I_LoopEndList make_I_LoopEndList(I_Operand loop_end_label);
void I_push_loop_end(I_Function func, I_Operand loop_end_label);
I_Operand I_pop_loop_end(I_Function func);
I_Function make_I_Function(S_Symbol name, F_Frame frame);
I_FunctionList make_I_FunctionList(I_Function func);
I_Function I_trans_function(TR_Function func);
I_FunctionList I_trans_prog(TR_Function prog);

I_StringLabel I_new_string_label();
I_Label I_new_label();
I_Temp I_new_temp(I_Function func);

void I_print_strings(I_Function func);
void I_print_prog(I_FunctionList prog);

string I_reg_name(I_Reg reg, int size);
