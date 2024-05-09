#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "frame.h"
#include "inst.h"
#include "types.h"
#include "util.h"

void I_print_instruction(I_Inst inst, int seq);

I_FunctionList functions;

I_Reg * I_named_regs;

const I_NamedReg callee_saved_regs[] = { RBX, R12, R13, R14, R15 };

const int I_num_callee_saved_regs = sizeof(callee_saved_regs) / sizeof(callee_saved_regs[0]);

const I_InstClass jump_classes[] = {
    I_JE,
    I_JNE,
    I_JL,
    I_JLE,
    I_JGE,
    I_JG
};

const I_InstClass jump_class_complements[] = {
    I_JNE,
    I_JE,
    I_JGE,
    I_JG,
    I_JL,
    I_JLE
};

I_Reg make_I_NamedReg(I_NamedReg reg) {
    I_Reg p = malloc_checked(sizeof(*p));
    p->kind = I_NAMED;
    p->u.reg = reg;
    return p;
}

I_Reg make_I_TempReg(I_Temp temp) {
    I_Reg p = malloc_checked(sizeof(*p));
    p->kind = I_TEMP;
    p->u.temp = temp;
    return p;
}

void I_init_named_regs() {
    I_named_regs = calloc(R15 + 1, sizeof(I_Reg));
    for (I_NamedReg reg_name = RAX; reg_name <= R15; reg_name++) {
        I_named_regs[reg_name] = make_I_NamedReg(reg_name);
    }
}

I_Mem make_I_MemFromIR(TR_Exp mem_exp, I_Reg named_base) {
    I_Mem p = malloc_checked(sizeof(*p));
    p->name = mem_exp->u.mem.name;
    p->base = named_base;
    p->index = NULL;
    p->scale = 0;
    p->offset = -mem_exp->u.mem.offset;
    return p;
}

I_Mem make_I_DerefMem(I_Reg named_base) {
    I_Mem p = malloc_checked(sizeof(*p));
    p->name = NULL;
    p->base = named_base;
    p->index = NULL;
    p->scale = 0;
    p->offset = 0;
    return p;
}

I_Mem make_I_IndexedMem(I_Reg base_reg, I_Reg index_temp, int scale) {
    I_Mem p = malloc_checked(sizeof(*p));
    p->name = NULL;
    p->base = base_reg;
    p->index = index_temp;
    p->scale = scale;
    p->offset = 0;
    return p;
}

I_Mem make_I_OffsetMem(I_Reg base_reg, int offset) {
    I_Mem p = malloc_checked(sizeof(*p));
    p->base = base_reg;
    p->index = 0;
    p->scale = 0;
    p->offset = offset;
    return p;
}

I_String make_I_String(string str, I_StringLabel label) {
    I_String p = malloc_checked(sizeof(*p));
    p->str = str;
    p->label = label;
    return p;
}

I_StringList make_I_StringList(I_String str) {
    I_StringList p = malloc_checked(sizeof(*p));
    p->head = str;
    p->tail = NULL;
    p->end = NULL;
    return p;
}

void I_add_string(I_Function func, I_String str) {
    I_StringList new_node = make_I_StringList(str);
    if (!func->strings) {
        func->strings = new_node;
        func->strings->end = func->strings;
    } else {
        func->strings->end->tail = new_node;
        func->strings->end = func->strings->end->tail;
    }
}

I_Operand make_I_ImmOperand(int num) {
    I_Operand p = malloc_checked(sizeof(*p));
    p->kind = I_IMM_OPRND;
    p->u.imm = num;
    return p;
}

I_Operand make_I_RegOperand(I_Reg reg) {
    I_Operand p = malloc_checked(sizeof(*p));
    p->kind = I_REG_OPRND;
    p->u.reg = reg;
    return p;
}

I_Operand make_I_MemOperand(I_Mem mem) {
    I_Operand p = malloc_checked(sizeof(*p));
    p->kind = I_MEM_OPRND;
    p->u.mem = mem;
    return p;
}

I_Operand make_I_StringLabelOperand(I_StringLabel string_label) {
    I_Operand p = malloc_checked(sizeof(*p));
    p->kind = I_STR_LABEL_OPRND;
    p->u.string_label = string_label;
    return p;
}

I_Operand make_I_LabelOperand(I_Label label) {
    I_Operand p = malloc_checked(sizeof(*p));
    p->kind = I_LABEL_OPRND;
    p->u.label = label;
    return p;
}

I_Operand make_I_TargetOperand(string target) {
    I_Operand p = malloc_checked(sizeof(*p));
    p->kind = I_TARGET_OPRND;
    p->u.target = target;
    return p;
}

I_Inst make_I_Inst(I_InstClass class, int size) {
    I_Inst p = malloc_checked(sizeof(*p));
    p->class = class;
    p->size = size;
    p->seq = -1;
    if (class > I_UNARY) {
        p->dst = NULL;
    }
    if (class > I_NULLARY) {
        p->src = NULL;
    }
    return p;
}

I_InstList make_I_InstList(I_Inst inst) {
    I_InstList p = malloc_checked(sizeof(*p));
    p->head = inst;
    p->tail = NULL;
    p->end = NULL;
    return p;
}

void I_add_label(I_Function func, I_Operand label_operand) {
    I_Inst inst = make_I_Inst(I_LABEL, 0);
    inst->src = label_operand;
    I_add_inst(func, inst);
}

void I_add_inst(I_Function func, I_Inst inst) {
    I_InstList new_node = make_I_InstList(inst);
    if (!func->instructions) {
        func->instructions = new_node;
        func->instructions->end = func->instructions;
    } else {
        func->instructions->end->tail = new_node;
        func->instructions->end =
            func->instructions->end->tail;
    }
}

void I_allocate(I_Function func, int size) {
    I_Inst inst = make_I_Inst(I_LOAD, T_INT_SIZE);
    inst->src = make_I_ImmOperand(size);
    inst->dst = make_I_RegOperand(I_named_regs[RDI]);
    I_add_inst(func, inst);
    inst = make_I_Inst(I_FCALL, T_POINTER_SIZE);
    inst->src = make_I_TargetOperand("malloc");
    I_add_inst(func, inst);
}

I_Operand I_trans_mem(I_Function func, TR_Exp mem_exp) {
    int nestings = func->nesting_level - mem_exp->u.mem.nesting_level;
    I_Inst inst = NULL;
    if (nestings) {
        inst = make_I_Inst(I_LOAD, T_POINTER_SIZE);
        inst->src = make_I_MemOperand(make_I_OffsetMem(I_named_regs[RBP], 0));
        inst->dst = make_I_RegOperand(I_named_regs[R15]);
        I_add_inst(func, inst);
    }
    for (int level = 1; level < nestings; ++level) {
        inst = make_I_Inst(I_LOAD, T_POINTER_SIZE);
        inst->src = make_I_MemOperand(make_I_OffsetMem(I_named_regs[R15], 0));
        inst->dst = make_I_RegOperand(I_named_regs[R14]);
        I_add_inst(func, inst);
        inst = make_I_Inst(I_LOAD, T_POINTER_SIZE);
        inst->src = make_I_RegOperand(I_named_regs[R14]);
        inst->dst = make_I_RegOperand(I_named_regs[R15]);
        I_add_inst(func, inst);
    }
    I_Mem simple_mem =
        make_I_MemFromIR(mem_exp, nestings ? I_named_regs[R15] : I_named_regs[RBP]);
    return make_I_MemOperand(simple_mem);
}

I_InstClass rel_jump(A_Oper op) {
    return jump_classes[op - A_EQ_OP];
}

I_InstClass complement_rel_jump(A_Oper op) {
    I_InstClass ret = jump_class_complements[op - A_EQ_OP];
    return ret;
}

I_Operand I_move_to_register(
        I_Function func,
        I_Operand in_operand,
        I_Reg reg,
        int size) {
    I_Inst inst = make_I_Inst(I_LOAD, size);
    inst->src = in_operand;
    inst->dst = make_I_RegOperand(reg);
    I_Operand out_operand = inst->dst;
    I_add_inst(func, inst);
    return out_operand;
}

I_Operand I_move_to_named_reg(
        I_Function func,
        I_Operand in_operand,
        I_NamedReg reg_name,
        int size
        ) {
    if (in_operand->kind == I_REG_OPRND &&
            in_operand->u.reg->kind == I_NAMED &&
            in_operand->u.reg->u.reg == reg_name) {
        return in_operand;
    }
    return I_move_to_register(
            func,
            in_operand,
            I_named_regs[reg_name],
            size
            );
}

I_Operand I_move_to_temp_reg(
        I_Function func,
        I_Operand in_operand,
        int size
        ) {
    if (in_operand->kind == I_REG_OPRND &&
            in_operand->u.reg->kind == I_TEMP) {
        return in_operand;
    }
    return I_move_to_register(
            func,
            in_operand,
            make_I_TempReg(I_new_temp(func)),
            size
            );
}

void I_add_cmp_inst(I_Function func, TR_Exp exp) {
    I_Inst inst = make_I_Inst(I_CMP, T_INT_SIZE);
    if (exp->u.rel.right->kind == TR_VAR_EXP) {
        inst->src = I_trans_exp(func, exp->u.rel.right->u.var);
    } else {
        inst->src = I_trans_exp(func, exp->u.rel.right);
    }
    inst->dst = I_move_to_temp_reg(
            func,
            I_trans_exp(func, exp->u.rel.left),
            exp->size
            );
    I_add_inst(func, inst);
}

I_Operand I_trans_exp(I_Function func, TR_Exp exp) {
    switch (exp->kind) {
        case TR_NUM_EXP:
            {
                return make_I_ImmOperand(exp->u.num);
            }
        case TR_MEM_EXP:
            {
                return I_trans_mem(func, exp);
            }
        case TR_STRING_EXP:
            {
                I_StringLabel new_label = I_new_string_label();
                I_String new_string = make_I_String(exp->u.str, new_label);
                I_add_string(func, new_string);
                return make_I_StringLabelOperand(new_label);
            }
        case TR_VAR_EXP:
            {
                return I_trans_exp(func, exp->u.var);
            }
        case TR_FIELD_EXP:
            {
                I_Operand var_mem_operand = I_trans_exp(func, exp->u.field.var);
                I_Inst inst = make_I_Inst(I_LOAD, T_POINTER_SIZE);
                inst->src = var_mem_operand;
                I_Reg base = make_I_TempReg(I_new_temp(func));
                inst->dst = make_I_RegOperand(base);
                I_add_inst(func, inst);
                return make_I_MemOperand(make_I_OffsetMem(base, exp->u.field.field_offset));
            }
        case TR_SUBSCRIPT_EXP:
            {
                I_Operand var_operand = I_trans_exp(func, exp->u.subscript.var);
                I_Reg base = make_I_TempReg(I_new_temp(func));
                I_Inst inst = make_I_Inst(I_LOAD, exp->size);
                inst->src = var_operand;
                inst->dst = make_I_RegOperand(base);
                I_add_inst(func, inst);
                I_Operand index_operand = I_trans_exp(func, exp->u.subscript.index);
                I_Mem mem = NULL;
                switch (index_operand->kind) {
                    case I_IMM_OPRND:
                        {
                            mem = make_I_OffsetMem(base, index_operand->u.imm * exp->size);
                            break;
                        }
                    case I_REG_OPRND:
                        {
                            mem = make_I_IndexedMem(base, index_operand->u.reg, exp->size);
                            break; 
                        }
                    case I_MEM_OPRND:
                        {
                            I_Inst inst = make_I_Inst(I_LOAD, T_INT_SIZE);
                            inst->src = index_operand;
                            I_Reg index_reg = make_I_TempReg(I_new_temp(func));
                            inst->dst = make_I_RegOperand(index_reg);
                            I_add_inst(func, inst);
                            mem = make_I_IndexedMem(base, index_reg, exp->size);
                            break;
                        }
                    default:
                        {
                            break;
                        }
                }
                return make_I_MemOperand(mem);
            }
        case TR_RECORD_EXP:
            {
                I_allocate(func, exp->size);
                int offset = 0;
                for (TR_ExpList exps = exp->u.record; exps; exps = exps->tail) {
                    I_Operand initializer = I_trans_exp(func, exps->head);
                    if (initializer->kind == I_MEM_OPRND) {
                        initializer = I_move_to_temp_reg(func, initializer, exps->head->size);
                    }
                    I_Inst inst = make_I_Inst(I_STORE, exps->head->size);
                    inst->src = initializer;
                    inst->dst = make_I_MemOperand(make_I_OffsetMem(I_named_regs[RAX], offset));
                    I_add_inst(func, inst);
                    offset += exps->head->size;
                }
                return make_I_RegOperand(I_named_regs[RAX]);
            }
        case TR_ARRAY_EXP:
            {
                I_Operand init_operand = I_trans_exp(func, exp->u.array.init);
                I_Operand num_elements_operand =
                    I_trans_exp(func, exp->u.array.num_elements);
                int element_size = exp->u.array.element_size;
                I_Operand element_size_operand = make_I_ImmOperand(element_size);
                I_Operand rdi =
                    I_move_to_named_reg(func, element_size_operand, RDI, T_INT_SIZE);
                I_Inst inst = make_I_Inst(I_MUL, T_INT_SIZE);
                inst->src = num_elements_operand;
                inst->dst = rdi;
                I_add_inst(func, inst);
                inst = make_I_Inst(I_FCALL, T_POINTER_SIZE);
                inst->src = make_I_TargetOperand("malloc");
                I_add_inst(func, inst);
                I_Operand iter_init_operand = make_I_ImmOperand(0);
                I_Operand iteration_operand =
                    I_move_to_temp_reg(func, iter_init_operand, T_INT_SIZE);
                I_Operand test_label_operand =
                    make_I_LabelOperand(I_new_label());
                I_Operand exit_label_operand =
                    make_I_LabelOperand(I_new_label());
                I_add_label(func, test_label_operand);
                inst = make_I_Inst(I_CMP, T_INT_SIZE);
                inst->src = num_elements_operand;
                inst->dst = iteration_operand;
                I_add_inst(func, inst);
                inst = make_I_Inst(I_JE, 0);
                inst->src = exit_label_operand;
                I_add_inst(func, inst);
                inst = make_I_Inst(I_STORE, element_size);
                inst->src = init_operand;
                inst->dst =
                    make_I_MemOperand(
                            make_I_IndexedMem(
                                I_named_regs[RAX],
                                iteration_operand->u.reg,
                                element_size
                                )
                            );
                I_add_inst(func, inst);
                inst = make_I_Inst(I_ADD, T_INT_SIZE);
                inst->src = make_I_ImmOperand(1);
                inst->dst = iteration_operand;
                I_add_inst(func, inst);
                inst = make_I_Inst(I_JMP, 0);
                inst->src = test_label_operand;
                I_add_inst(func, inst);
                inst = make_I_Inst(I_JMP, 0);
                inst->src = test_label_operand;
                I_add_label(func, exit_label_operand);
                return make_I_RegOperand(I_named_regs[RAX]);
            }
        case TR_ARITH_OP_EXP:
            {
                I_Inst inst = NULL;
                // In subtraction (ATT/GCC), sub y x means x - y.
                TR_Exp left = exp->u.arith.left;
                TR_Exp right = exp->u.arith.right;
                if (exp->u.arith.op == A_MINUS_OP) {
                    TR_Exp tmp = left;
                    left = right;
                    right = tmp;
                }
                // Micro-optimization: if the left operand is a variable,
                // use its memory address directly.
                TR_Exp left_exp = left->kind == TR_VAR_EXP ?  left->u.var : left;
                I_Operand left_operand = I_trans_exp(func, left_exp);
                I_Operand right_operand = I_trans_exp(func, right);
                if (right_operand->kind != I_REG_OPRND) {
                    inst = make_I_Inst(I_LOAD, exp->size);
                    inst->src = right_operand;
                    inst->dst = make_I_RegOperand(make_I_TempReg(I_new_temp(func)));
                    right_operand = inst->dst;
                    I_add_inst(func, inst);
                }
                switch (exp->u.arith.op) {
                    case A_PLUS_OP:
                        {
                            inst = make_I_Inst(I_ADD, exp->size);
                            break;
                        }
                    case A_MINUS_OP:
                        {
                            inst = make_I_Inst(I_SUB, exp->size);
                            break;
                        }
                    case A_TIMES_OP:
                        {
                            if (left_operand->kind == I_IMM_OPRND) {
                                inst = make_I_Inst(I_LOAD, exp->size);
                                inst->src = left_operand;
                                inst->dst = make_I_RegOperand(make_I_TempReg(I_new_temp(func)));
                                left_operand = inst->dst;
                                I_add_inst(func, inst);
                            }
                            inst = make_I_Inst(I_MUL, exp->size);
                            break;
                        }
                    default:
                        {
                            break;
                        }
                }
                inst->src = left_operand;
                inst->dst = right_operand;
                I_add_inst(func, inst);
                return right_operand;
            }
        case TR_DIV_OP_EXP:
            {
                I_Inst inst = make_I_Inst(I_LOAD, T_INT_SIZE);
                inst->src = I_trans_exp(func, exp->u.div.left);
                I_Operand dividend_operand = make_I_RegOperand(I_named_regs[RAX]);
                inst->dst = dividend_operand;
                I_add_inst(func, inst);
                I_Operand divisor_operand = I_trans_exp(func, exp->u.div.right);
                if (divisor_operand->kind == I_IMM_OPRND) {
                    inst = make_I_Inst(I_LOAD, T_INT_SIZE);
                    inst->src = divisor_operand;
                    inst->dst = make_I_RegOperand(make_I_TempReg(I_new_temp(func)));
                    I_add_inst(func, inst);
                    divisor_operand = inst->dst;
                }
                I_add_inst(func, make_I_Inst(I_EXTEND, 0));
                inst = make_I_Inst(I_DIV, T_INT_SIZE);
                inst->src = divisor_operand;
                I_add_inst(func, inst);
                return dividend_operand;
            }
        case TR_REL_OP_EXP:
            {
                I_add_cmp_inst(func, exp);
                I_Operand false_label_operand =
                    make_I_LabelOperand(I_new_label());
                I_Operand result_reg_operand =
                    make_I_RegOperand(make_I_TempReg(I_new_temp(func)));
                I_Inst inst = make_I_Inst(complement_rel_jump(exp->u.rel.op), 0);
                inst->src = false_label_operand;
                I_add_inst(func, inst);
                inst = make_I_Inst(I_LOAD, T_INT_SIZE);
                inst->src = make_I_ImmOperand(1);
                inst->dst = result_reg_operand;
                I_add_inst(func, inst);
                I_Operand join_label_operand =
                    make_I_LabelOperand(I_new_label());
                inst = make_I_Inst(I_JMP, 0);
                inst->src = join_label_operand;
                I_add_inst(func, inst);
                I_add_label(func, false_label_operand);
                inst = make_I_Inst(I_LOAD, T_INT_SIZE);
                inst->src = make_I_ImmOperand(0);
                inst->dst = result_reg_operand;
                I_add_inst(func, inst);
                I_add_label(func, join_label_operand);
                return result_reg_operand;
            }
        case TR_LOG_OP_EXP:
            {
                I_Operand left_operand = I_trans_exp(func, exp->u.log.left);
                I_Operand right_operand = I_trans_exp(func, exp->u.log.right);
                right_operand = I_move_to_temp_reg(func, right_operand, exp->size);
                I_Inst inst = make_I_Inst(exp->u.log.op == A_AND_OP ? I_AND : I_OR, exp->size);
                inst->src = left_operand;
                inst->dst = right_operand;
                I_add_inst(func, inst);
                return inst->dst;
            }
        case TR_IF_ELSE_EXP:
            {
                I_Operand test_result = I_trans_exp(func, exp->u.if_else.test);
                if (test_result->kind != I_REG_OPRND) {
                    test_result =
                        I_move_to_temp_reg(func, test_result, exp->u.if_else.test->size);
                }
                I_Inst inst = make_I_Inst(I_TEST, exp->u.if_else.test->size);
                inst->src = inst->dst = test_result;
                I_add_inst(func, inst);
                I_Operand false_label_operand =
                    make_I_LabelOperand(I_new_label());
                inst = make_I_Inst(I_JZ, 0);
                inst->src = false_label_operand;
                I_add_inst(func, inst);
                I_Operand true_value_operand =
                    I_trans_exp(func, exp->u.if_else.true_branch);
                I_Operand result_operand = NULL;
                if (true_value_operand->kind == I_REG_OPRND) {
                    result_operand = true_value_operand;
                } else {
                    inst = make_I_Inst(I_LOAD, exp->u.if_else.true_branch->size);
                    inst->src = true_value_operand;
                    inst->dst = make_I_RegOperand(make_I_TempReg(I_new_temp(func)));
                    result_operand = inst->dst;
                    I_add_inst(func, inst);
                }
                I_Operand join_label_operand =
                    make_I_LabelOperand(I_new_label());
                inst = make_I_Inst(I_JMP, 0);
                inst->src = join_label_operand;
                I_add_inst(func, inst);
                I_add_label(func, false_label_operand);
                I_Operand false_value_operand =
                    I_trans_exp(func, exp->u.if_else.false_branch);
                inst = make_I_Inst(I_LOAD, exp->u.if_else.false_branch->size);
                inst->src = false_value_operand;
                inst->dst = result_operand;
                I_add_inst(func, inst);
                I_add_label(func, join_label_operand);
                return result_operand;
            }
        case TR_FCALL_EXP:
            {
                I_NamedReg arg_regs[] = { RDI, RSI, RDX, RCX, R8, R9 };
                int i = 0;
                for (TR_ExpList args = exp->u.fcall.args; args; args = args->tail, ++i) {
                    I_Inst inst = make_I_Inst(I_LOAD, args->head->size);
                    inst->src = I_trans_exp(func, args->head);
                    inst->dst = make_I_RegOperand(I_named_regs[arg_regs[i]]);
                    I_add_inst(func, inst);
                }
                I_Inst inst = make_I_Inst(I_FCALL, 0);
                char * name = S_name(exp->u.fcall.name);
                if (!strcmp(name, "getchar")) {
                    name = "getchar_";
                }
                inst->src = make_I_TargetOperand(name);
                I_add_inst(func, inst);
                I_Operand ret_operand = make_I_RegOperand(I_named_regs[RAX]);
                return I_move_to_temp_reg(func, ret_operand, exp->size);
            }
        case TR_SEQ_EXP:
            {
                TR_StmList stms = exp->u.seq;
                for (; stms->tail; stms = stms->tail) {
                    I_trans_stm(func, stms->head);
                }
                return I_trans_exp(func, stms->head->u.exp);
            }
        default:
            {
                return NULL;
            }
    }
}

void I_trans_stm(I_Function func, TR_Stm stm) {
    I_Inst inst = NULL;
    switch (stm->kind) {
        case TR_ASSIGN_STM:
            {
                inst = make_I_Inst(I_STORE, stm->u.assign.var->size);
                inst->src = I_trans_exp(func, stm->u.assign.value);
                inst->dst = I_trans_exp(func, stm->u.assign.var);
                if (inst->src->kind == I_MEM_OPRND && inst->dst->kind == I_MEM_OPRND) {
                    inst->src =
                        I_move_to_register(func, inst->src, make_I_TempReg(I_new_temp(func)),
                                stm->u.assign.var->size);
                }
                I_add_inst(func, inst);
                break;
            }
        case TR_PCALL_STM:
            {
                I_NamedReg arg_regs[] = { RDI, RSI, RDX, RCX, R8, R9 };
                int i = 0;
                for (TR_ExpList args = stm->u.pcall.args; args; args = args->tail, ++i) {
                    I_Inst inst = make_I_Inst(I_LOAD, args->head->size);
                    inst->src = I_trans_exp(func, args->head);
                    inst->dst = make_I_RegOperand(I_named_regs[arg_regs[i]]);
                    I_add_inst(func, inst);
                }
                I_Inst inst = make_I_Inst(I_PCALL, T_POINTER_SIZE);
                inst->src = make_I_TargetOperand(S_name(stm->u.pcall.name));
                I_add_inst(func, inst);
                break;
            }
        case TR_SEQ_STM:
            {
                TR_StmList stms = stm->u.seq;
                for (; stms; stms = stms->tail) {
                    I_trans_stm(func, stms->head);
                }
                break;
            }
        case TR_IF_STM:
            {
                I_Operand test_result = I_trans_exp(func, stm->u.if_.test);
                if (test_result->kind != I_REG_OPRND) {
                    test_result =
                        I_move_to_temp_reg(func, test_result, stm->u.if_.test->size);
                }
                I_Inst inst = make_I_Inst(I_TEST, stm->u.if_.test->size);
                inst->src = inst->dst = test_result;
                I_add_inst(func, inst);
                I_Operand false_label_operand =
                    make_I_LabelOperand(I_new_label());
                inst = make_I_Inst(I_JZ, 0);
                inst->src = false_label_operand;
                I_add_inst(func, inst);
                I_trans_stm(func, stm->u.if_.true_branch);
                I_add_label(func, false_label_operand);
                break;
            }
        case TR_IF_ELSE_STM:
            {
                I_Operand test_result = I_trans_exp(func, stm->u.if_else.test);
                if (test_result->kind != I_REG_OPRND) {
                    test_result =
                        I_move_to_temp_reg(func, test_result, stm->u.if_else.test->size);
                }
                I_Inst inst = make_I_Inst(I_TEST, stm->u.if_else.test->size);
                inst->src = inst->dst = test_result;
                I_add_inst(func, inst);
                I_Operand false_label_operand =
                    make_I_LabelOperand(I_new_label());
                inst = make_I_Inst(I_JZ, 0);
                inst->src = false_label_operand;
                I_add_inst(func, inst);
                I_trans_stm(func, stm->u.if_else.true_branch);
                I_Operand join_label_operand =
                    make_I_LabelOperand(I_new_label());
                inst = make_I_Inst(I_JMP, 0);
                inst->src = join_label_operand;
                I_add_inst(func, inst);
                I_add_label(func, false_label_operand);
                I_trans_stm(func, stm->u.if_else.false_branch);
                I_add_label(func, join_label_operand);
                break;
            }
        case TR_WHILE_STM:
            {
                I_Operand exit_label_operand =
                    make_I_LabelOperand(I_new_label());
                I_push_loop_end(func, exit_label_operand);
                I_Operand test_label_operand =
                    make_I_LabelOperand(I_new_label());
                I_add_label(func, test_label_operand);
                I_Operand test_result = I_trans_exp(func, stm->u.while_.test);
                if (test_result->kind == I_IMM_OPRND) {
                    test_result =
                        I_move_to_temp_reg(func, test_result, stm->u.while_.test->size);
                }
                I_Inst inst = make_I_Inst(I_TEST, stm->u.while_.test->size);
                inst->src = inst->dst = test_result;
                I_add_inst(func, inst);
                inst = make_I_Inst(I_JZ, 0);
                inst->src = exit_label_operand;
                I_add_inst(func, inst);
                I_trans_stm(func, stm->u.while_.body);
                inst = make_I_Inst(I_JMP, 0);
                inst->src = test_label_operand;
                I_add_inst(func, inst);
                I_add_label(func, exit_label_operand);
                I_pop_loop_end(func);
                break;
            }
        case TR_FOR_STM:
            {
                I_Operand exit_label_operand =
                    make_I_LabelOperand(I_new_label());
                I_push_loop_end(func, exit_label_operand);
                inst = make_I_Inst(I_STORE, stm->u.for_.var->size);
                inst->src = I_trans_exp(func, stm->u.for_.lo);
                inst->dst = I_trans_exp(func, stm->u.for_.var);
                I_add_inst(func, inst);
                I_Operand test_label_operand =
                    make_I_LabelOperand(I_new_label());
                I_add_label(func, test_label_operand);
                TR_Exp test_exp =
                    make_TR_RelOpExp(
                            stm->u.for_.var,
                            stm->u.for_.hi,
                            A_LT_OP
                            );
                I_Operand test_result = I_trans_exp(func, test_exp);
                if (test_result->kind == I_IMM_OPRND) {
                    test_result =
                        I_move_to_temp_reg(func, test_result, T_INT_SIZE);
                }
                I_Inst inst = make_I_Inst(I_TEST, T_INT_SIZE);
                inst->src = inst->dst = test_result;
                I_add_inst(func, inst);
                inst = make_I_Inst(I_JZ, 0);
                inst->src = exit_label_operand;
                I_add_inst(func, inst);
                I_trans_stm(func, stm->u.for_.body);
                TR_Stm inc_stm =
                    make_TR_AssignStm(
                            make_TR_ArithOpExp(
                                stm->u.for_.var,
                                make_TR_NumExp(1),
                                A_PLUS_OP
                                ),
                            stm->u.for_.var
                            );
                I_trans_stm(func, inc_stm);
                inst = make_I_Inst(I_JMP, 0);
                inst->src = test_label_operand;
                I_add_inst(func, inst);
                I_add_label(func, exit_label_operand);
                I_pop_loop_end(func);
                break;
            }
        case TR_BREAK_STM:
            {
                inst = make_I_Inst(I_JMP, 0);
                inst->src = func->loop_end_labels->head;
                I_add_inst(func, inst);
                break;
            }
        case TR_EXP_STM:
            {
                I_Operand last_exp = I_trans_exp(func, stm->u.exp);
                I_move_to_named_reg(func, last_exp, RAX, stm->u.exp->size);
                break;
            }
        default:
            {
                break;
            }
    }
}

I_LoopEndList make_I_LoopEndList(I_Operand loop_end_label) {
    I_LoopEndList p = malloc_checked(sizeof(*p));
    p->head = loop_end_label;
    p->tail = NULL;
    return p;
}

void I_push_loop_end(I_Function func, I_Operand loop_end_label) {
    I_LoopEndList new_node = make_I_LoopEndList(loop_end_label);
    new_node->tail = func->loop_end_labels;
    func->loop_end_labels = new_node;
}

I_Operand I_pop_loop_end(I_Function func) {
    if (!func->loop_end_labels) {
        return NULL;
    }
    I_Operand latest_loop_end = func->loop_end_labels->head;
    func->loop_end_labels = func->loop_end_labels->tail;
    return latest_loop_end;
}

int I_align_for_stack(int size) {
    return (size / F_STACK_ALIGNMENT  +
            (size % F_STACK_ALIGNMENT) ? 1 : 0) *
        F_STACK_ALIGNMENT;
}

I_Function make_I_Function(S_Symbol name, F_Frame frame) {
    I_Function p = malloc_checked(sizeof(*p));
    p->name = name;
    p->frame_size = frame->end;
    p->nesting_level = frame->nesting_level;
    p->instructions = NULL;
    p->loop_end_labels = NULL;
    p->temp = 0;
    p->padding = 0;
    return p;
}

I_FunctionList make_I_FunctionList(I_Function func) {
    I_FunctionList p = malloc_checked(sizeof(*p));
    p->head = func;
    p->tail = NULL;
    return p;
}

I_FunctionList I_add_function(I_FunctionList funcs, I_Function func) {
    I_FunctionList new_node = make_I_FunctionList(func);
    if (!funcs) {
        funcs = new_node;
    } else {
        funcs->tail = new_node;
    }
    return funcs;
}

void I_add_prologue(TR_Function func, I_Function trans_func) {
    I_Inst inst = make_I_Inst(I_PUSH, T_POINTER_SIZE);
    inst->src = make_I_RegOperand(I_named_regs[RBP]);
    I_add_inst(trans_func, inst);
    inst = make_I_Inst(I_LOAD, T_POINTER_SIZE);
    inst->src = make_I_RegOperand(I_named_regs[RSP]);
    inst->dst = make_I_RegOperand(I_named_regs[RBP]);
    I_add_inst(trans_func, inst);
    trans_func->effective_frame_size = trans_func->frame_size;
    int remainder = trans_func->effective_frame_size % F_STACK_ALIGNMENT;
    trans_func->effective_frame_size += remainder ? F_STACK_ALIGNMENT - remainder : 0;
    trans_func->aligned_frame_size =
        trans_func->effective_frame_size;
    inst = make_I_Inst(I_SUB, T_POINTER_SIZE);
    inst->src = make_I_ImmOperand(trans_func->aligned_frame_size);
    inst->dst = make_I_RegOperand(I_named_regs[RSP]);
    I_add_inst(trans_func, inst);

}

void I_add_epilogue(TR_Function func, I_Function trans_func) {
    I_Inst inst = make_I_Inst(I_ADD, T_POINTER_SIZE);
    inst->src = make_I_ImmOperand(trans_func->aligned_frame_size);
    inst->dst = make_I_RegOperand(I_named_regs[RSP]);
    I_add_inst(trans_func, inst);
    inst = make_I_Inst(I_POP, T_POINTER_SIZE);
    inst->src = make_I_RegOperand(I_named_regs[RBP]);
    I_add_inst(trans_func, inst);
    inst = make_I_Inst(I_RET, T_POINTER_SIZE);
    I_add_inst(trans_func, inst);
}

void I_trans_params(TR_Function func, I_Function trans_func) {
    I_NamedReg param_regs[] = { RDI, RSI, RDX, RCX, R8, R9 };
    int i = 0;
    int offset = 0;
    F_VarList params = func->frame->params;
    for (; params; params = params->tail, ++i) {
        F_Var param = params->head;
        offset += T_size(param->type);
        I_Inst inst = make_I_Inst(I_STORE, T_size(param->type));
        inst->src = make_I_RegOperand(I_named_regs[param_regs[i]]);
        inst->dst = make_I_MemOperand(make_I_OffsetMem(I_named_regs[RBP], -offset));
        I_add_inst(trans_func, inst);
    }
}

I_Function I_trans_function(TR_Function func) {
    I_Function new_function = make_I_Function(func->name, func->frame);
    I_add_prologue(func, new_function);
    I_trans_params(func, new_function);
    for (TR_StmList stms = func->body; stms; stms = stms->tail) {
        I_trans_stm(new_function, stms->head);
    }
    I_add_epilogue(func, new_function);
    return new_function;
}

void I_trans_and_add_function(TR_Function func) {
    I_FunctionList new_node = make_I_FunctionList(I_trans_function(func));
    if (!functions) {
        functions = new_node;
    } else {
        I_FunctionList current = functions;
        while (current->tail) {
            current = current->tail;
        }
        current->tail = new_node;
    }
}

void I_trans_prog_impl(TR_Function func) {
    I_trans_and_add_function(func);
    for (TR_FunctionList flist = func->children; flist; flist = flist->tail) {
        I_trans_prog_impl(flist->head);
    }
}

I_FunctionList I_trans_prog(TR_Function prog) {
    I_init_named_regs();
    I_trans_prog_impl(prog);
    return functions;
}

I_StringLabel I_new_string_label() {
    static I_StringLabel label = 0;
    return label++;
}

I_Temp I_new_temp(I_Function func) {
    return func->temp++;
}

I_Label I_new_label() {
    static I_Label label = 0;
    return label++;
}


void I_print_strings(I_Function func) {
    for (I_StringList list = func->strings; list; list = list->tail) {
        printf("%d - %s\n", list->head->label, list->head->str);
    }
}

string I_reg_name(I_Reg reg, int size) {
    string named_64[] = {
        "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp",
        "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"
    };
    string named_32[] = {
        "eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp",
        "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"
    };
    return (size == T_POINTER_SIZE) ? named_64[reg->u.reg] : named_32[reg->u.reg];
}

void I_print_reg_name(I_Reg reg, int size) {
    if (reg->kind == I_NAMED) {
        printf("%%%s", I_reg_name(reg, size));
    } else {
        printf("T%d", reg->u.temp);
    }
}

void I_print_operand(I_Operand operand, int size) {
    switch (operand->kind) {
        case I_IMM_OPRND:
            {
                printf("$%d", operand->u.imm);
                break;
            }
        case I_REG_OPRND:
            {
                I_print_reg_name(operand->u.reg, size);
                break;
            }
        case I_MEM_OPRND:
            {
                if (operand->u.mem->offset) {
                    printf("%d", operand->u.mem->offset);
                }
                putchar('(');
                I_print_reg_name(operand->u.mem->base, T_POINTER_SIZE);
                if (operand->u.mem->index) {
                    putchar(',');
                    I_print_reg_name(operand->u.mem->index, size);
                }
                if (operand->u.mem->scale) {
                    putchar(',');
                    printf("%d", operand->u.mem->scale);
                }
                putchar(')');
                break;
            }
        case I_STR_LABEL_OPRND:
            {
                printf("L%d", operand->u.string_label);
                break;
            }
        case I_LABEL_OPRND:
            {
                printf("L%d", operand->u.label);
                break;
            }
        case I_TARGET_OPRND:
            {
                printf("%s", operand->u.target);
                break;
            }
        default:
            {
                break;
            }
    }
}

void I_print_instruction(I_Inst inst, int seq) {
    string classes[] = {
        "binary",
        "load",
        "store",
        "add",
        "sub",
        "mul",
        "cmp",
        "test",
        "and",
        "or",
        "unary",
        "div",
        "call", // PCALL
        "call", // FCALL
        "jmp",
        "je",
        "jne",
      "jl",
        "jle",
        "jge",
        "jg",
        "jnz",
        "jz",
        "push",
        "pop",
        "label",
        "nullary",
        "extend",
        "ret"
    };
    printf("%d ", seq);
    I_InstClass class = inst->class;
    if (class == I_LABEL) {
        printf("L%d:", inst->src->u.label);
    } else {
        printf("%s %d ", classes[class], inst->size);
        if (class < I_NULLARY) {
            I_print_operand(inst->src, inst->size);
        }
        if (class < I_UNARY) {
            putchar(' ');
            I_print_operand(inst->dst, inst->size);
        }
    }
    putchar('\n');
}

void I_print_instructions(I_Function func) {
    int seq = 0;
    for (I_InstList insts = func->instructions; insts; insts = insts->tail) {
        I_print_instruction(insts->head, seq++);
    }
}

void I_print_prog(I_FunctionList prog) {
    for (I_FunctionList funcs = prog; funcs; funcs = funcs->tail) {
        printf("Function: %s\n\n", S_name(funcs->head->name));
        if (funcs->head->strings) {
            puts("Strings:");
            I_print_strings(funcs->head);
            putchar('\n');
        }
        puts("Instructions:");
        I_print_instructions(funcs->head);
        if (funcs->tail) {
            puts("\n----------------------------\n");
        }
    }
}

