/*
 * frame.c
 * Implementation of the Frame abstraction,
 * representing a function's stack frame.
 * Author: Amittai Aviram - aviram@bc.edu.
 */

#include <stdio.h>
#include <stdlib.h>

#include "frame.h"
#include "util.h"

F_Frame make_F_Frame(int nesting_level) {
    F_Frame p = malloc_checked(sizeof(*p));
    p->nesting_level = nesting_level;
    p->params = NULL;
    p->vars = NULL;
    p->end = 0;
    return p;
}

F_Var make_F_Var(S_Symbol name, T_Type type) {
    F_Var p = malloc_checked(sizeof(*p));
    p->name = name;
    p->type = type;
    return p;
}

F_VarList make_F_VarList(F_Var head, F_VarList tail) {
    F_VarList p = malloc_checked(sizeof(*p));
    p->head = head;
    p->tail = tail;
    return p;
}

void F_add_param(F_Frame frame, F_Var param) {
    F_VarList new_param = make_F_VarList(param, NULL);
    if (!frame->params) {
        frame->params = new_param;
    } else {
        F_VarList params = frame->params;
        while (params->tail) {
            params = params->tail;
        }
        params->tail = new_param;
    }
    frame->end += T_size(param->type);
}

void F_add_var(F_Frame frame, F_Var var) {
    F_VarList new_var = make_F_VarList(var, NULL);
    if (!frame->vars) {
        frame->vars = new_var;
    } else {
        F_VarList vars = frame->vars;
        while (vars->tail) {
            vars = vars->tail;
        }
        vars->tail = new_var;
    }
    frame->end += T_size(var->type);
}

void F_print_frame(F_Frame frame) {
    printf("\tNesting level: %d\n", frame->nesting_level);
    if (frame->params) {
        puts("\tParameters:");
        for (F_VarList params = frame->params; params; params = params->tail) {
            printf("\t\t%s : %s (%d)\n",
                    S_name(params->head->name),
                    T_type_name(params->head->type),
                    T_size(params->head->type));
        }
    }
    if (frame->vars) {
        puts("\tLocal variables:");
        for (F_VarList vars = frame->vars; vars; vars = vars->tail) {
            printf("\t\t%s : %s (%d)\n",
                    S_name(vars->head->name),
                    T_type_name(vars->head->type),
                    T_size(vars->head->type));
        }
    }
}
