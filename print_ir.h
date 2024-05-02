/*
 * print_ir.h
 * API of facilities for printing the IR representation
 * of tne input Tiger program after translation to
 * the AST and then to the IR.
 * Author: Amittai Aviram - aviram@bc.edu.
 */

#pragma once

#include "translate.h"

void P_print_ir(TR_Function main_);
void P_print_function_body(TR_Function func);
