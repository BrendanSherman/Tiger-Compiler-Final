#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "frame.h"
#include "inst.h"
#include "regs.h"

/*
 * In Instructions module:
 * In every function, save callee-saved registers to the stack,
 * as part of the prologue. Restore at the end.
 *
 * In Registers module:
 * Before every call, push every caller-saved register 
 * currently in use.
 * After the call, pop all caller-saved registers —
 * -- Imediately after a procedure call
 * -- After the return value is stored for a function call.
 */

bool R_in_use[R15 + 1];
bool R_pushed[R15 + 1];
R_RegAlloc * R_reg_allocs;

// Registers ordered with caller-saved registers first.
const I_NamedReg regs[] = { RCX, RSI, RDI, R8, R9, R10, R11, RBX, R12, R13, R14, R15 };
const int I_num_regs = sizeof(regs) / sizeof(regs[0]);

bool R_is_callee_saved(I_NamedReg reg) {
    return reg >= RAX && reg <= RBX;
}

bool R_is_caller_saved(I_NamedReg reg) {
    return !R_is_callee_saved(reg);
}

void R_init(I_Function func) {
    R_reg_allocs = calloc(func->temp, sizeof(R_reg_allocs[0]));
    for (int i = 0; i <= R15; ++i) {
        R_in_use[i] = false;
        R_pushed[i] = false;
    }
}

void R_clear(I_Function func) {
    for (int i = 0; i < func->temp; ++i) {
        if (R_reg_allocs[i]) {
            // free(R_reg_allocs[i]);
        }
    }
    // free(R_reg_allocs);
    R_reg_allocs = NULL;
    for (int i = 0; i <= R15; ++i) {
        R_in_use[i] = false;
        R_pushed[i] = false;
    }
}

R_RegAlloc make_R_RegAlloc(int inst_seq) {
    R_RegAlloc p = malloc_checked(sizeof(*p));
    p->first_use = p->last_use = inst_seq;
    p->reg = RNONE;
    return p;
}

I_Temp R_get_temp_if_exists(I_Operand operand) {
    if (!operand) {
        return -1;
    }
    if (operand->kind == I_REG_OPRND && operand->u.reg->kind == I_TEMP) {
        return operand->u.reg->u.temp;
    }
    return -1;
}

I_Temp R_get_temps_from_mem(I_Operand operand, I_Temp * index_temp) {
    *index_temp = -1;
    if (!operand || operand->kind != I_MEM_OPRND) {
        return -1;
    }
    if (operand->u.mem->index &&
            operand->u.mem->index->kind == I_TEMP) {
        *index_temp = operand->u.mem->index->u.temp;
    }
    if (operand->u.mem->base &&
            operand->u.mem->base->kind == I_TEMP) {
        return operand->u.mem->base->u.temp;
    }
    return -1;
}

I_NamedReg R_get_named_reg_if_exists(I_Operand operand) {
    if (operand && operand->kind == I_REG_OPRND && operand->u.reg->kind == I_NAMED) {
        return operand->u.reg->u.reg;
    }
    return RNONE;
}

I_NamedReg R_take_named_reg(I_Temp temp) {
    for (int i = 0; i < I_num_regs; ++i) {
        if (!R_in_use[regs[i]]) {
            R_in_use[regs[i]] = true;
            return regs[i];
        }
    }
    fprintf(stderr, "Exhausted registers.\n");
    return RNONE;
}

void R_release_named_reg(I_NamedReg reg) {
    R_in_use[reg] = false;
}

void R_replace_temp(I_Inst inst, I_NamedReg reg) {
    if (inst->src && inst->src->kind == I_REG_OPRND && inst->src->u.reg->kind == I_TEMP) {
        inst->src = make_I_RegOperand(make_I_NamedReg(reg));
    } else if (inst->dst && inst->dst->kind == I_REG_OPRND && inst->dst->u.reg->kind == I_TEMP) {
        inst->dst = make_I_RegOperand(make_I_NamedReg(reg));
    }
}

void R_replace_temp_in_mem(I_Operand operand, I_NamedReg reg, I_Temp temp) {
    if (operand->kind != I_MEM_OPRND) {
        fprintf(stderr, "Wrong argument for replacing temp in operand.\n");
        return;
    }
    if (operand->u.mem->base &&
            operand->u.mem->base->kind == I_TEMP &&
            operand->u.mem->base->u.temp == temp
            ) {
        operand->u.mem->base = make_I_NamedReg(reg);
    } else if (operand->u.mem->index &&
            operand->u.mem->index->kind == I_TEMP &&
            operand->u.mem->index->u.temp == temp
            ) {
        operand->u.mem->index = make_I_NamedReg(reg);
    }
    else {
        fprintf(stderr, "Temp not found in memory expression.\n");
    }
}

void R_number_instructions(I_Function func) {
    int seq = 0;
    for (I_InstList insts = func->instructions; insts; insts = insts->tail) {
        I_Inst inst = insts->head;
        inst->seq = seq++;
    }
}

void R_analyze_temp_usage(I_Function func) {
    for (I_InstList insts = func->instructions; insts; insts = insts->tail) {
        I_Inst inst = insts->head;
        I_Temp dst_temp = R_get_temp_if_exists(inst->dst);
        if (dst_temp >= 0) {
            if (!R_reg_allocs[dst_temp]) {
                R_reg_allocs[dst_temp] = make_R_RegAlloc(inst->seq);
            }
            else {
                R_reg_allocs[dst_temp]->last_use = inst->seq;
            }
        } 
        I_Temp src_temp = R_get_temp_if_exists(inst->src);
        if (src_temp >= 0) {
            R_reg_allocs[src_temp]->last_use = inst->seq;
        }
        I_Temp index_temp;
        I_Temp base_temp = R_get_temps_from_mem(inst->src, &index_temp);
        if (base_temp >= 0) {
            R_reg_allocs[base_temp]->last_use = inst->seq;
        }
        if (index_temp >= 0) {
            R_reg_allocs[index_temp]->last_use = inst->seq;
        }
        base_temp = R_get_temps_from_mem(inst->dst, &index_temp);
        if (base_temp >= 0) {
            R_reg_allocs[base_temp]->last_use = inst->seq;
        }
        if (index_temp >= 0) {
            R_reg_allocs[index_temp]->last_use = inst->seq;
        }
    }
}

void R_alloc_named_reg(I_Inst inst, I_Temp temp) {
    if (R_reg_allocs[temp]) {
        if (R_reg_allocs[temp]->reg != RNONE) {
            R_replace_temp(inst, R_reg_allocs[temp]->reg);
            if (R_reg_allocs[temp]->last_use == inst->seq) {
                R_release_named_reg(R_reg_allocs[temp]->reg);
            }
        } else if (R_reg_allocs[temp]->first_use == inst->seq) {
            R_reg_allocs[temp]->reg = R_take_named_reg(temp);
            R_replace_temp(inst, R_reg_allocs[temp]->reg);
        }
    }
}

void R_alloc_named_reg_in_mem(I_Operand operand, I_Temp temp, int seq) {
    if (R_reg_allocs[temp]) {
        if (R_reg_allocs[temp]->reg != RNONE) {
            R_replace_temp_in_mem(operand, R_reg_allocs[temp]->reg, temp);
            if (R_reg_allocs[temp]->last_use == seq) {
                R_release_named_reg(R_reg_allocs[temp]->reg);
            }
        } else if (R_reg_allocs[temp]->first_use == seq) {
            R_reg_allocs[temp]->reg = R_take_named_reg(temp);
            R_replace_temp_in_mem(operand, R_reg_allocs[temp]->reg, temp);
        } 
    }
}

I_InstList R_insert_reg_pushes(I_InstList insts, I_Function func) {
    I_InstList tail = insts->tail;
    int caller_saved_regs_in_use = 0;
    for (int i = 0; R_is_caller_saved(regs[i]); ++i) {
        if (R_in_use[regs[i]]) {
            I_Inst inst = make_I_Inst(I_PUSH, T_POINTER_SIZE);
            inst->src = make_I_RegOperand(make_I_NamedReg(regs[i]));
            insts->tail = make_I_InstList(inst);
            insts = insts->tail;
            R_pushed[regs[i]] = true;
            R_in_use[regs[i]] = false;
            ++caller_saved_regs_in_use;
        }
    }
    int remainder = func->effective_frame_size % F_STACK_ALIGNMENT;
    func->padding = remainder ? F_STACK_ALIGNMENT - remainder : 0;
    if (func->padding) {
        I_Inst inst = make_I_Inst(I_SUB, T_POINTER_SIZE);
        inst->src = make_I_ImmOperand(func->padding);
        inst->dst = make_I_RegOperand(make_I_NamedReg(RSP));
        insts->tail = make_I_InstList(inst);
        insts = insts->tail;
    }
    insts->tail = tail;
    return insts;
}

I_InstList R_insert_reg_pops(I_InstList insts, I_Function func) {
    I_InstList tail = insts->tail;
    if (func->padding) {
        I_Inst inst = make_I_Inst(I_ADD, T_POINTER_SIZE);
        inst->src = make_I_ImmOperand(func->padding);
        inst->dst = make_I_RegOperand(make_I_NamedReg(RSP));
        insts->tail = make_I_InstList(inst);
        insts = insts->tail;
    }
    int i = 0;
    while (R_is_caller_saved(regs[i])) {
        ++i;
    }
    --i;
    for (; i >= 0; --i) {
        if (R_pushed[regs[i]]) {
            I_Inst inst = make_I_Inst(I_POP, T_POINTER_SIZE);
            inst->src = make_I_RegOperand(make_I_NamedReg(regs[i]));
            insts->tail = make_I_InstList(inst);
            insts = insts->tail;
            R_in_use[regs[i]] = true;
            R_pushed[regs[i]] = false;
        }
    }
    insts->tail = tail;
    func->padding = 0;
    return insts;
}

void R_allocate_named_regs(I_Function func) {
    for (I_InstList insts = func->instructions; insts; insts = insts->tail) {
        I_Inst inst = insts->head;
        if (inst->class == I_FCALL || inst->class == I_PCALL) {
            insts = R_insert_reg_pops(insts, func);
        }
        I_Temp dst = R_get_temp_if_exists(inst->dst);
        I_Temp src = R_get_temp_if_exists(inst->src);
        if (dst >= 0) {
            R_alloc_named_reg(inst, dst);
        }
        if (src >= 0) {
            R_alloc_named_reg(inst, src);
        }
        I_Temp index_temp;
        I_Temp base_temp = R_get_temps_from_mem(inst->src, &index_temp);
        if (base_temp >= 0) {
            R_alloc_named_reg_in_mem(inst->src, base_temp, inst->seq);
        }
        if (index_temp >= 0) {
            R_alloc_named_reg_in_mem(inst->src, index_temp, inst->seq);
        }
        base_temp = R_get_temps_from_mem(inst->dst, &index_temp);
        if (base_temp >= 0) {
            R_alloc_named_reg_in_mem(inst->dst, base_temp, inst->seq);
        }
        if (index_temp >= 0) {
            R_alloc_named_reg_in_mem(inst->dst, index_temp, inst->seq);
        }
        I_InstList tail = insts->tail;
        if (tail && (tail->head->class == I_FCALL || tail->head->class == I_PCALL)) {
            insts = R_insert_reg_pushes(insts, func);
        }
    }
}

void R_remove_vacuous_loads(I_Function func) {
    I_InstList insts = func->instructions;
    I_InstList pred = NULL;
    for (; insts && insts->head; pred = insts, insts = insts->tail) {
        I_Inst inst = insts->head;
        if (inst->class == I_LOAD &&
                inst->src->kind == I_REG_OPRND &&
                inst->dst->kind == I_REG_OPRND &&
                inst->src->u.reg->u.reg == inst->dst->u.reg->u.reg) {
            pred->tail = insts->tail;
        }
    }
}

void R_print_analysis(I_Function func) {
    for (int i = 0; i < func->temp; ++i) {
        if (R_reg_allocs[i]->first_use) {
            printf("T%d: First use: %d  Last use: %d\n", 
                    i, R_reg_allocs[i]->first_use, R_reg_allocs[i]->last_use);
        }
    }
}

void R_analyze_and_allocate(I_Function func) {
    R_init(func);
    R_number_instructions(func);
    R_analyze_temp_usage(func);
    // R_print_analysis(func);
    R_allocate_named_regs(func);
    R_remove_vacuous_loads(func);
    R_clear(func);
}

void R_allocate_regs(I_FunctionList funcs) {
    for (; funcs; funcs = funcs->tail) {
        R_analyze_and_allocate(funcs->head);
    }
}

