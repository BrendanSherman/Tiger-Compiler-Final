OUT=makefile
CC='gcc'
FLAGS="-Wall -Werror -std=c99 -D_XOPEN_SOURCE=700 -g"
MODULES=(\
    compile \
    code_gen \
    regs \
	print_ir \
	prabsyn \
	inst \
	semant \
	translate \
	frame \
	env \
	types \
	absyn \
	symbol \
	table \
	y.tab \
	lex.yy \
	errormsg \
	util \
)

rm -f ${OUT}

echo "CC = ${CC}" >> ${OUT}
echo "FLAGS = ${FLAGS}" >> ${OUT}
echo >> ${OUT}

printf %s "compile:" >> ${OUT}
for MODULE in ${MODULES[@]}
do
    printf %s " ${MODULE}.o" >> ${OUT}
done
echo >> ${OUT}
echo "\t"'${CC} ${FLAGS} $^ -o $@' >> ${OUT}
echo >> ${OUT}

for MODULE in ${MODULES[@]}
do
    printf %s "${MODULE}.o: ${MODULE}.c" >> ${OUT}
    if [[ "${MODULE}" != "lex.yy" ]]
    then
        printf %s " ${MODULE}.h" >> ${OUT}
    fi
    if [[ "${MODULE}" == "compile" ]]
    then
        printf %s " y.tab.h" >> ${OUT}
    fi
    echo >> ${OUT}
    echo "\t"'${CC} ${FLAGS} -c $<' >> ${OUT}
    echo >> ${OUT}
done

echo "lex.yy.c: tiger.lex" >> ${OUT}
echo "\t"'lex $<' >> ${OUT}
echo >> ${OUT}

echo "y.tab.c: tiger.grm" >> ${OUT}
echo "\t"'bison -dv $< -o $@' >> ${OUT}
echo >> ${OUT}

echo "y.tab.h: y.tab.c" >> ${OUT}
echo >> ${OUT}

echo "clean:" >> ${OUT}
echo "\t"'rm -f compile *.o lex.yy.c y.tab.* y.output' >> ${OUT}
echo >> ${OUT}

