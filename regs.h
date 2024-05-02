#pragma once

#include "inst.h"

typedef struct R_RegAlloc_ * R_RegAlloc;
typedef struct R_DefUseList_ * R_DefUseList;

struct R_RegAlloc_ {
    int first_use;
    int last_use;
    I_NamedReg reg;
};

struct R_DefUse {
    int def;
    int use;
};

struct R_DefUseList_ {
    struct R_DefUse head;
    R_DefUseList tail;
};

void R_allocate_regs(I_FunctionList funcs);
