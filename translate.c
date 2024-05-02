#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "env.h"
#include "symbol.h"
#include "translate.h"
#include "types.h"
#include "util.h"

TR_StmList TR_loop_list;

TR_TransExp make_TR_TransFunction(TR_Function function) {
    TR_TransExp p = malloc_checked(sizeof(*p));
    p->kind = TR_FUNCTION;
    p->u.function = function;
    return p;
}

TR_TransExp make_TR_TransStm(TR_Stm stm) {
    TR_TransExp p = malloc_checked(sizeof(*p));
    p->kind = TR_STM;
    p->u.stm = stm;
    return p;
}

TR_TransExp make_TR_TransExp(TR_Exp exp) {
    TR_TransExp p = malloc_checked(sizeof(*p));
    p->kind = TR_EXP;
    p->u.exp = exp;
    return p;
}

TR_Function make_TR_Function(S_Symbol name, F_Frame frame) {
    TR_Function p = malloc_checked(sizeof(*p));
    p->name = name;
    p->frame = frame;
    p->parent = NULL;
    p->children = NULL;
    p->body = NULL;
    return p;
}

TR_FunctionList make_TR_FunctionList(TR_Function head, TR_FunctionList tail) {
    TR_FunctionList p = malloc_checked(sizeof(*p));
    p->head = head;
    p->tail = tail;
    return p;
}

void TR_append_function(TR_Function parent, TR_Function child) {
    TR_FunctionList new_list = make_TR_FunctionList(child, NULL);
    if (!parent->children) {
        parent->children = new_list;
    } else {
        TR_FunctionList flist = parent->children;
        while (flist->tail) {
            flist = flist->tail;
        }
        flist->tail = new_list;
    }
    assert(parent->frame);
    int nesting_level = parent->frame->nesting_level + 1;
    if (!child->frame) {
        child->frame = make_F_Frame(nesting_level);
    } else {
        child->frame->nesting_level = nesting_level;
    }
    child->parent = parent;
}

void TR_add_param(TR_Function func, S_Symbol name, T_Type type) {
    assert(func->frame);
    F_add_param(func->frame, make_F_Var(name, type));
}

void TR_add_var(TR_Function func, S_Symbol name, T_Type type) {
    assert(func->frame);
    F_add_var(func->frame, make_F_Var(name, type));
}

void TR_add_stm_to_function(TR_Function func, TR_Stm stm) {
    if (stm->kind == TR_SEQ_STM) {
        TR_add_stm_list_to_function(func, stm->u.seq);
        return;
    }
    TR_StmList new_node = make_TR_StmList(stm);
    if (!func->body) {
        func->body = new_node;
    } else {
        TR_StmList current = func->body;
        while (current->tail) {
            current = current->tail;
        }
        current->tail = new_node;
    }
}

void TR_add_stm_list_to_function(TR_Function func, TR_StmList stms) {
    while (stms) {
        TR_add_stm_to_function(func, stms->head);
        stms = stms->tail;
    }
}

TR_Stm make_TR_AssignStm(TR_Exp value, TR_Exp var) {
    TR_Stm p = malloc_checked(sizeof(*p));
    p->kind = TR_ASSIGN_STM;
    p->u.assign.value = value;
    p->u.assign.var = var;
    return p;
}

TR_Stm make_TR_PCallStm(S_Symbol name, TR_ExpList args) {
    TR_Stm p = malloc_checked(sizeof(*p));
    p->kind = TR_PCALL_STM;
    p->u.pcall.name = name;
    p->u.pcall.args = args;
    return p;
}

TR_Stm make_TR_SeqStm(TR_StmList stms) {
    TR_Stm p = malloc_checked(sizeof(*p));
    p->kind = TR_SEQ_STM;
    p->u.seq = stms;
    return p;
}

TR_Stm make_TR_IfStm(TR_Exp test, TR_Stm true_branch) {
    TR_Stm p = malloc_checked(sizeof(*p));
    p->kind = TR_IF_STM;
    p->u.if_.test = test;
    p->u.if_.true_branch = true_branch;
    return p;
}

TR_Stm make_TR_IfElseStm(TR_Exp test, TR_Stm true_branch, TR_Stm false_branch) {
    TR_Stm p = malloc_checked(sizeof(*p));
    p->kind = TR_IF_ELSE_STM;
    p->u.if_else.test = test;
    p->u.if_else.true_branch = true_branch;
    p->u.if_else.false_branch = false_branch;
    return p;
}

TR_Stm make_TR_WhileStm(TR_Exp test, TR_Stm body) {
    TR_Stm p = malloc_checked(sizeof(*p));
    p->kind = TR_WHILE_STM;
    p->u.while_.test = test;
    p->u.while_.body = body;
    return p;
}

TR_Stm make_TR_ForStm(TR_Exp var, TR_Exp lo, TR_Exp hi, TR_Stm body) {
    TR_Stm p = malloc_checked(sizeof(*p));
    p->kind = TR_FOR_STM;
    p->u.for_.var = var;
    p->u.for_.lo = lo;
    p->u.for_.hi = hi;
    p->u.for_.body = body;
    return p;
}

TR_Stm make_TR_BreakStm() {
    TR_Stm p = malloc_checked(sizeof(*p));
    p->kind = TR_BREAK_STM;
    return p;
}

TR_Stm make_TR_ExpStm(TR_Exp exp) {
    TR_Stm p = malloc_checked(sizeof(*p));
    p->kind = TR_EXP_STM;
    p->u.exp = exp;
    return p;
}

TR_StmList make_TR_StmList(TR_Stm head) {
    TR_StmList p = malloc_checked(sizeof(*p));
    p->head = head;
    p->tail = NULL;
    return p;
}

TR_StmList TR_add_stm(TR_StmList list, TR_Stm stm) {
    TR_StmList new_list = make_TR_StmList(stm);
    if (!list) {
        return new_list;
    }
    TR_StmList current = list;
    while (current->tail) {
        current = current->tail;
    }
    current->tail = new_list;
    return list;
}


TR_Exp make_TR_NumExp(int num) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->kind = TR_NUM_EXP;
    p->size = T_INT_SIZE;
    p->u.num = num;
    return p;
}

TR_Exp make_TR_StringExp(string str) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->size = T_POINTER_SIZE;
    p->kind = TR_STRING_EXP;
    p->u.str = str;
    return p;
}

TR_Exp make_TR_MemExp(S_Table venv, S_Symbol sym) {
    TR_Exp p = malloc_checked(sizeof(*p));
    E_EnvEntry var_entry = S_look(venv, sym);
    assert(var_entry);
    assert(var_entry->kind == E_VAR_ENTRY);
    p->kind = TR_MEM_EXP;
    p->size = T_size(var_entry->u.var.type);
    p->u.mem.name = sym;
    p->u.mem.nesting_level = var_entry->u.var.nesting_level;
    p->u.mem.offset = var_entry->u.var.offset;
    return p;
}

TR_Exp make_TR_VarExp(TR_Exp var) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->size = var->size;
    p->kind = TR_VAR_EXP;
    p->u.var = var;
    return p;
}

TR_Exp make_TR_FieldExp(TR_Exp var, S_Symbol field_name, int field_size, int field_offset) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->kind = TR_FIELD_EXP;
    p->size = field_size;
    p->u.field.var = var;
    p->u.field.field_name = field_name;
    p->u.field.field_offset = field_offset;
    return p;
}

TR_Exp make_TR_SubscriptExp(TR_Exp var, int element_size, TR_Exp index) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->kind = TR_SUBSCRIPT_EXP;
    p->size = element_size;
    p->u.subscript.var = var;
    p->u.subscript.index = index;
    return p;
}

TR_Exp make_TR_RecordExp(int size, TR_ExpList inits) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->kind = TR_RECORD_EXP;
    p->size = size;
    p->u.record = inits;
    return p;
}

TR_Exp make_TR_ArrayExp(TR_Exp num_elements, int element_size, TR_Exp init) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->kind = TR_ARRAY_EXP;
    p->size = T_POINTER_SIZE;
    p->u.array.num_elements = num_elements;
    p->u.array.element_size = element_size;
    p->u.array.init = init;
    return p;
}

TR_Exp make_TR_ArithOpExp(TR_Exp left, TR_Exp right, A_Oper op) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->size = right->size;
    p->kind = TR_ARITH_OP_EXP;
    p->u.arith.left = left;
    p->u.arith.right = right;
    p->u.arith.op = op;
    return p;
}

TR_Exp make_TR_DivOpExp(TR_Exp left, TR_Exp right) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->size = right->size;
    p->kind = TR_DIV_OP_EXP;
    p->u.div.left = left;
    p->u.div.right = right;
    return p;
}

TR_Exp make_TR_RelOpExp(TR_Exp left, TR_Exp right, A_Oper op) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->size = T_INT_SIZE;
    p->kind = TR_REL_OP_EXP;
    p->u.rel.left = left;
    p->u.rel.right = right;
    p->u.rel.op = op;
    return p;
}

TR_Exp make_TR_LogOpExp(TR_Exp left, TR_Exp right, A_Oper op) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->size = T_INT_SIZE;
    p->kind = TR_LOG_OP_EXP;
    p->u.log.left = left;
    p->u.log.right = right;
    p->u.log.op = op;
    return p;
}


TR_Exp make_TR_IfElseExp(TR_Exp test, TR_Exp true_branch, TR_Exp false_branch) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->kind = TR_IF_ELSE_EXP;
    p->u.if_else.test = test;
    p->u.if_else.true_branch = true_branch;
    p->u.if_else.false_branch = false_branch;
    return p;
}

TR_Exp make_TR_FCallExp(S_Table venv, S_Symbol name, TR_ExpList args) {
    TR_Exp p = malloc_checked(sizeof(*p));
    E_EnvEntry func_entry = S_look(venv, name);
    assert(func_entry);
    assert(func_entry->kind = E_FUN_ENTRY);
    assert(func_entry->u.fun.result);
    p->size = T_size(func_entry->u.fun.result);
    p->kind = TR_FCALL_EXP;
    p->u.fcall.name = name;
    p->u.fcall.args = args;
    return p;
}

TR_Exp make_TR_SeqExp(TR_StmList stms) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->kind = TR_SEQ_EXP;
    p->u.seq = stms;
    return p;
}

TR_Stm TR_convert_seq_exp_to_stm(TR_Exp seq) {
    TR_Stm p = malloc_checked(sizeof(*p));
    p->kind = TR_SEQ_STM;
    p->u.seq = seq->u.seq;
    return p;
}

TR_Exp TR_convert_seq_stm_to_exp(TR_Stm seq, int size) {
    TR_Exp p = malloc_checked(sizeof(*p));
    p->kind = TR_SEQ_EXP;
    p->size = size;
    p->u.seq = seq->u.seq;
    return p;
}


TR_ExpList make_TR_ExpList(TR_Exp head, TR_ExpList tail) {
    TR_ExpList p = malloc_checked(sizeof(*p));
    p->head = head;
    p->tail = tail;
    return p;
}

TR_ExpList TR_add_exp(TR_ExpList list, TR_Exp exp) {
    TR_ExpList new_list = make_TR_ExpList(exp, NULL);
    if (!list) {
        return new_list;
    }
    TR_ExpList current = list;
    while (current->tail) {
        current = current->tail;
    }
    current->tail = new_list;
    return list;
}

void TR_push_loop(TR_Stm loop) {
    TR_StmList new_node = make_TR_StmList(loop);
    if (TR_loop_list) {
        new_node->tail = TR_loop_list;
    }
    TR_loop_list = new_node;
}

void TR_pop_loop() {
    if (TR_loop_list) {
        TR_loop_list = TR_loop_list->tail;
    }
}

void TR_print_function(TR_Function func) {
    if (!func) {
        return;
    }
    printf("Function: %s\n", S_name(func->name));
    if (func->parent) {
        printf("\tParent: %s\n", S_name(func->parent->name));
    }
    if (func->frame) {
        F_print_frame(func->frame);
    }
}
