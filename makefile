CC = gcc
FLAGS = -Wall -Werror -std=c99 -D_XOPEN_SOURCE=700 -g

compile: compile.o code_gen.o regs.o print_ir.o prabsyn.o inst.o semant.o translate.o frame.o env.o types.o absyn.o symbol.o table.o y.tab.o lex.yy.o errormsg.o util.o
	${CC} ${FLAGS} $^ -o $@

compile.o: compile.c compile.h y.tab.h
	${CC} ${FLAGS} -c $<

code_gen.o: code_gen.c code_gen.h
	${CC} ${FLAGS} -c $<

regs.o: regs.c regs.h
	${CC} ${FLAGS} -c $<

print_ir.o: print_ir.c print_ir.h
	${CC} ${FLAGS} -c $<

prabsyn.o: prabsyn.c prabsyn.h
	${CC} ${FLAGS} -c $<

inst.o: inst.c inst.h
	${CC} ${FLAGS} -c $<

semant.o: semant.c semant.h
	${CC} ${FLAGS} -c $<

translate.o: translate.c translate.h
	${CC} ${FLAGS} -c $<

frame.o: frame.c frame.h
	${CC} ${FLAGS} -c $<

env.o: env.c env.h
	${CC} ${FLAGS} -c $<

types.o: types.c types.h
	${CC} ${FLAGS} -c $<

absyn.o: absyn.c absyn.h
	${CC} ${FLAGS} -c $<

symbol.o: symbol.c symbol.h
	${CC} ${FLAGS} -c $<

table.o: table.c table.h
	${CC} ${FLAGS} -c $<

y.tab.o: y.tab.c y.tab.h
	${CC} ${FLAGS} -c $<

lex.yy.o: lex.yy.c
	${CC} ${FLAGS} -c $<

errormsg.o: errormsg.c errormsg.h
	${CC} ${FLAGS} -c $<

util.o: util.c util.h
	${CC} ${FLAGS} -c $<

lex.yy.c: tiger.lex
	lex $<

y.tab.c: tiger.grm
	bison -dv $< -o $@

y.tab.h: y.tab.c

clean:
	rm -f compile *.o lex.yy.c y.tab.* y.output

