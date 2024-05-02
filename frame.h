/*
 * frame.h
 * Definition of the Frame abstraction,
 * representing a function's stack frame.
 * Author: Amittai Aviram - aviram@bc.edu.
 */
#pragma once

#include "symbol.h"
#include "types.h"

#define F_STACK_ALIGNMENT 16

typedef struct F_Frame_ * F_Frame;
typedef struct F_Var_ * F_Var;
typedef struct F_VarList_ * F_VarList;

struct F_Frame_ {
    int nesting_level;
    F_VarList params;
    F_VarList vars;
    int end;
};

struct F_Var_ {
    S_Symbol name;
    T_Type type;
};

struct F_VarList_ {
    F_Var head;
    F_VarList tail;
};

F_Frame make_F_Frame(int nesting_level);
F_Var make_F_Var(S_Symbol name, T_Type type);
F_VarList make_F_VarList(F_Var head, F_VarList tail);
void F_add_param(F_Frame frame, F_Var param);
void F_add_var(F_Frame frame, F_Var var);
void F_print_frame(F_Frame frame);

