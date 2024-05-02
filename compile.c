/*
 * parse.c - Parse source file.
 * Run ./parse <Tiger-source-code-file>
 * to see visual representations of the Tiger code.
 * This version prints the output type and IR by default.
 * Use the -p flag at the end of the command
 * to print the AST before the type and IR.
 * Orig. author: Andrew Appel.
 * Revised by Amittai Aviram - aviram@bc.edu.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "absyn.h"
#include "code_gen.h"
#include "compile.h"
#include "errormsg.h"
#include "inst.h"
#include "prabsyn.h"
#include "print_ir.h"
#include "regs.h"
#include "semant.h"
#include "symbol.h"
#include "util.h"
#include "y.tab.h"

extern A_Exp absyn_root;

/* Parse source file fname; 
 * return abstract syntax data structure.
 */
A_Exp parse(string fname) {
    EM_reset(fname);
    if (!yyparse()) {
        return absyn_root;
    } else {
        fprintf(stderr, "Parsing failed\n");
        return NULL;
    }
}

int main(int argc, char ** argv) {
    if (argc < 2) {
        fprintf(stderr,"usage: %s filename\n", argv[0]);
        exit(EXIT_FAILURE);
    }
    A_Exp program = parse(argv[1]);
    if (program) {
        if (argc == 3 && !strcmp(argv[2], "-p")) {
                puts("\nAbstract syntax:\n");
                pr_exp(stdout, program, 0);
                puts("\n");
        }
        SEM_ExpType prog_exp_type = SEM_trans_prog(program);
        if (prog_exp_type) {
            // printf("Type: %s\n", T_type_name(prog_exp_type->type));
            // P_print_ir(prog_exp_type->exp->u.function);
            // putchar('\n');
            I_FunctionList funcs = I_trans_prog(prog_exp_type->exp->u.function);
            // I_print_prog(funcs);
            // putchar('\n');
            R_allocate_regs(funcs);
            // I_print_prog(funcs);
            CG_emit_prog(funcs);
        } else {
            puts("Type could not be established.");
        }
    }
    return EXIT_SUCCESS;
}
