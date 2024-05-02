#include <stdio.h>

#include "inst.h"
#include "types.h"

void CG_emit_strings(I_Function func) {
    puts(".section .rodata");
    for (I_StringList list = func->strings; list; list = list->tail) {
        printf("LSTR%d:\n.asciz \"%s\"\n", list->head->label, list->head->str);
    }
}

void CG_emit_reg_name(I_Reg reg, int size) {
    if (reg->kind == I_NAMED) {
        printf("%%%s", I_reg_name(reg, size));
    } else {
        printf("T%d", reg->u.temp);
    }
}

void CG_emit_operand(I_Operand operand, int size) {
    switch (operand->kind) {
        case I_IMM_OPRND:
            {
                printf("$%d", operand->u.imm);
                break;
            }
        case I_REG_OPRND:
            {
                CG_emit_reg_name(operand->u.reg, size);
                break;
            }
        case I_MEM_OPRND:
            {
                if (operand->u.mem->offset) {
                    printf("%d", operand->u.mem->offset);
                }
                putchar('(');
                CG_emit_reg_name(operand->u.mem->base, T_POINTER_SIZE);
                if (operand->u.mem->index) {
                    putchar(',');
                    CG_emit_reg_name(operand->u.mem->index, T_POINTER_SIZE);
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
                printf("$LSTR%d", operand->u.string_label);
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

string CG_suffix(int size) {
    switch (size) {
        case T_INT_SIZE:
            return "l";
        case T_POINTER_SIZE:
            return "q";
        default:
            return "";
    }
}

void CG_emit_instruction(I_Inst inst, int seq) {
    string classes[] = {
        "binary",
        "mov", // LOAD
        "mov", // STORE
        "add",
        "sub",
        "imul",
        "cmp",
        "test",
        "and",
        "or",
        "unary",
        "idiv",
        "call", // PCALL
        "call", // FCALL
        "jmp",
        "je",
        "jne",
        "jlt",
        "jle",
        "jge",
        "jgt",
        "jnz",
        "jz",
        "push",
        "pop",
        "label",
        "nullary",
        "cltd",
        "ret"
    };
    I_InstClass class = inst->class;
    if (class == I_LABEL) {
        printf("L%d:", inst->src->u.label);
    } else {
        printf("%s%s ", classes[class], CG_suffix(inst->size));
        if (class < I_NULLARY) {
            CG_emit_operand(inst->src, inst->size);
        }
        if (class < I_UNARY) {
            printf(", ");
            CG_emit_operand(inst->dst, inst->size);
        }
    }
    putchar('\n');
}

void CG_emit_instructions(I_Function func) {
    int seq = 0;
    for (I_InstList insts = func->instructions; insts; insts = insts->tail) {
        CG_emit_instruction(insts->head, seq++);
    }
}

void CG_emit_prog(I_FunctionList prog) {
    for (I_FunctionList funcs = prog; funcs; funcs = funcs->tail) {
        if (funcs->head->strings) {
            CG_emit_strings(funcs->head);
        }
        puts(".text");
        printf(".globl %s\n", S_name(funcs->head->name));
        printf("%s:\n", S_name(funcs->head->name));
        CG_emit_instructions(funcs->head);
        putchar('\n');
    }
}


