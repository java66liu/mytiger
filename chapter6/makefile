a.out: parse.o lex.yy.o errormsg.o util.o absyn.o symbol.o table.o types.o semant.o env.o
	cc -g parse.o lex.yy.o errormsg.o util.o absyn.o symbol.o table.o types.o semant.o env.o

parse.o: parse.c errormsg.h util.h
	cc -g -c parse.c

absyn.o: absyn.c
	cc -g -c absyn.c

symbol.o: symbol.c
	cc -g -c symbol.c

table.o: table.c
	cc -g -c table.c

y.tab.o: y.tab.c
	cc -g -c y.tab.c

y.tab.c: tiger.y
	yacc -v tiger.y

errormsg.o: errormsg.c errormsg.h util.h
	cc -g -c errormsg.c

lex.yy.o: lex.yy.c errormsg.h util.h y.tab.c
	cc -g -c lex.yy.c

lex.yy.c: tiger.lex
	lex tiger.lex

util.o: util.c util.h
	cc -g -c util.c

types.o: types.c types.h
	cc -g -c types.c

semant.o: semant.c semant.h
	cc -g -c semant.c

env.o: env.c env.h
	cc -g -c env.c

clean: 
	rm -f a.out util.o parse.o lex.yy.o lex.yy.c errormsg.o y.tab.c y.tab.h y.tab.o absyn.o symbol.o table.o y.output types.o semant.o env.o
