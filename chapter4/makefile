a.out: parse.o lex.yy.o errormsg.o util.o absyn.o symbol.o table.o prabsyn.o
	cc -g parse.o lex.yy.o errormsg.o util.o absyn.o symbol.o table.o prabsyn.o

parse.o: parse.c errormsg.h util.h
	cc -g -c parse.c

absyn.o: absyn.c
	cc -g -c absyn.c

symbol.o: symbol.c
	cc -g -c symbol.c

table.o: table.c
	cc -g -c table.c

prabsyn.o: prabsyn.c
	cc -g -c prabsyn.c

y.tab.o: y.tab.c
	cc -g -c y.tab.c

y.tab.c: tiger.y
	yacc -dv tiger.y

y.tab.h: y.tab.c
	echo "y.tab.h was created at the same time as y.tab.c"

errormsg.o: errormsg.c errormsg.h util.h
	cc -g -c errormsg.c

lex.yy.o: lex.yy.c y.tab.h errormsg.h util.h
	cc -g -c lex.yy.c

lex.yy.c: tiger.lex
	lex tiger.lex

util.o: util.c util.h
	cc -g -c util.c

clean: 
	rm -f a.out util.o parse.o lex.yy.o lex.yy.c errormsg.o y.tab.c y.tab.h y.tab.o absyn.o symbol.o table.o prabsyn.o
