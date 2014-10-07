/*
 * parse.c - Parse source file.
 */

#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "symbol.h"
#include "absyn.h"
#include "errormsg.h"
#include "parse.h"

extern int yyparse(void);
extern A_exp absyn_root;

/* parse source file fname; 
   return abstract syntax data structure */
A_exp parse(string fname) {
	EM_reset(fname);
	if (yyparse() == 0) /* parsing worked */
		return absyn_root;
	else
		return NULL;
}

void parseVar(A_var v) {

}

void parseDec(A_dec d) {
    switch (d->kind) {
        case A_varDec:
            printf("%s\t", S_name(d->u.var.var));
            printf("%s\t", S_name(d->u.var.typ));
            parseExp(d->u.var.init);
            break;
        case A_functionDec:
            break;
        case A_typeDec:
            break;
        default:
            assert(0);
    }
}

void parseExp(A_exp e) {
    switch (e->kind) {
        case A_ifExp:
            printf("if\t");
            parseExp(e->u.iff.test);
            parseExp(e->u.iff.then);
            parseExp(e->u.iff.elsee);
            break;
        case A_arrayExp:
            printf("%s\t", S_name(e->u.array.typ));
            parseExp(e->u.array.size);
            parseExp(e->u.array.init);
            break;
        case A_assignExp:
            printf("assign\t");
            parseVar(e->u.assign.var);
            parseExp(e->u.assign.exp);
            break;
        case A_breakExp:
            printf("break\t");
            break;
        case A_callExp:
            printf("%s\t", S_name(e->u.call.func));
            A_expList el = e->u.call.args;
            while (el) {
                parseExp(el->head);
                el = el->tail;
            }
            break;
        case A_forExp:
            printf("for %s\t", S_name(e->u.forr.var));
            parseExp(e->u.forr.body);
            parseExp(e->u.forr.hi);
            parseExp(e->u.forr.lo);
            break;
        case A_intExp:
            printf("int\t");
            break;
        case A_letExp:
            printf("let\t");
            A_decList dl = e->u.let.decs;
            while (dl) {
                parseDec(dl->head);
                dl = dl->tail;
            }
            break;
        case A_nilExp:
            printf("nil\t");
            break;
        case A_opExp:
            printf("op\t");
            break;
        case A_recordExp:
            printf("record\t");
            break;
        case A_seqExp:
            printf("seq\t");
            break;
        case A_stringExp:
            printf("string\t");
            break;
        case A_varExp:
            printf("var\t");
            break;
        case A_whileExp:
            printf("while\t");
            break;
        default:
            assert(0);
    }
}

int main(int argc, char **argv) {
	if (argc != 2) {
		fprintf(stderr,"usage: a.out filename\n");
		exit(1);
	}
    parseExp(parse(argv[1]));
	return 0;
}
