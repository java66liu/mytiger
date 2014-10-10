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
	switch (v->kind) {
		case A_simpleVar:
			printf("%s", S_name(v->u.simple));
			break;
		case A_fieldVar:
			parseVar(v->u.field.var);
			printf(".%s", S_name(v->u.field.sym));
			break;
		case A_subscriptVar:
			parseVar(v->u.subscript.var);
			printf("[");
			parseExp(v->u.subscript.exp);
			printf("]");
			break;
		default:
			assert(0);
	}
}

void parseDec(A_dec d) {
	switch (d->kind) {
		case A_varDec:
			if (d->u.var.typ == NULL) {
				printf("var %s := ", S_name(d->u.var.var));
			}
			else {
				printf("var %s:%s := ", S_name(d->u.var.var), S_name(d->u.var.typ));
			}
			parseExp(d->u.var.init);
			printf("\n");
			break;
		case A_functionDec: {
			A_fundecList fdl = d->u.function;
			A_fieldList fl;
			while (fdl) {
				printf("function %s(", S_name(fdl->head->name));
				fl = fdl->head->params;
				while (fl) {
					printf("%s:%s", S_name(fl->head->name), S_name(fl->head->typ));
					if (fl->tail) {
						printf(", ");
					}
					fl = fl->tail;
				}
				printf(")");
				if (fdl->head->result) {
					printf(":%s", S_name(fdl->head->result));
				}
				printf(" =\n");
				if (fdl->head->body->kind == A_seqExp) {
					printf("(");
					parseExp(fdl->head->body);
					printf(")");
				}
				else {
					parseExp(fdl->head->body);
				}

				printf("\n");
				fdl = fdl->tail;
			}
			break;
		}
		case A_typeDec: {
			A_nametyList ntl = d->u.type;
			while (ntl) {
				printf("type %s = ", S_name(ntl->head->name));
				parseTy(ntl->head->ty);
				printf("\n");
				ntl = ntl->tail;
			}
			break;
		}
		default:
			assert(0);
	}
}

void parseExp(A_exp e) {
	switch (e->kind) {
		case A_ifExp:
			printf("if ");
			parseExp(e->u.iff.test);
			printf(" then\n");
			parseExp(e->u.iff.then);
			if (e->u.iff.elsee) {
				printf("\nelse\n");
				parseExp(e->u.iff.elsee);
			}
			break;
		case A_arrayExp:
			printf("%s [", S_name(e->u.array.typ));
			parseExp(e->u.array.size);
			printf("] of ");
			parseExp(e->u.array.init);
			break;
		case A_assignExp:
			parseVar(e->u.assign.var);
			printf(" := ");
			parseExp(e->u.assign.exp);
			break;
		case A_breakExp:
			printf("break\t");
			break;
		case A_callExp: {
			printf("%s(", S_name(e->u.call.func));
			A_expList el = e->u.call.args;
			while (el) {
				parseExp(el->head);
				if (el->tail) {
					printf(", ");
				}
				el = el->tail;
			}
			printf(")");
			break;
		}
		case A_forExp:
			printf("for %s := ", S_name(e->u.forr.var));
			parseExp(e->u.forr.lo);
			printf(" to ");
			parseExp(e->u.forr.hi);
			printf(" do\n");
			printf("(\n");
			parseExp(e->u.forr.body);
			printf(")");
			break;
		case A_intExp:
			printf("%d", e->u.intt);
			break;
		case A_letExp: {
			printf("let\n");
			A_decList dl = e->u.let.decs;
			while (dl) {
				parseDec(dl->head);
				dl = dl->tail;
			}
			printf("in\n");
			parseExp(e->u.let.body);
			printf("\nend\n");
			break;
		}
		case A_nilExp:
			printf("nil");
			break;
		case A_opExp:
			parseExp(e->u.op.left);
			switch (e->u.op.oper) {
				case A_plusOp:
					printf(" + ");
					break;
				case A_minusOp:
					printf(" - ");
					break;
				case A_timesOp:
					printf(" * ");
					break;
				case A_divideOp:
					printf(" / ");
					break;
				case A_eqOp:
					printf(" = ");
					break;
				case A_neqOp:
					printf(" != ");
					break;
				case A_ltOp:
					printf(" > ");
					break;
				case A_leOp:
					printf(" >= ");
					break;
				case A_gtOp:
					printf(" < ");
					break;
				case A_geOp:
					printf(" <= ");
					break;
				default:
					assert(0);
			}
			parseExp(e->u.op.right);
			break;
		case A_recordExp:
			printf("%s ", S_name(e->u.record.typ));
			A_efieldList efl = e->u.record.fields;
			printf("{ ");
			while (efl) {
				printf("%s=", S_name(efl->head->name));
				parseExp(efl->head->exp);
				if (efl->tail) {
					printf(", ");
				}
				efl = efl->tail;
			}
			printf(" }");
			break;
		case A_seqExp: {
			A_expList el = e->u.seq;
			while (el) {
				parseExp(el->head);
				if (el->tail) {
					printf(";\n");
				}
				el = el->tail;
			}
			break;
		}
		case A_stringExp:
			printf("\"%s\"", e->u.stringg);
			break;
		case A_varExp:
			parseVar(e->u.var);
			break;
		case A_whileExp:
			printf("while (");
			parseExp(e->u.whilee.test);
			printf(") do (\n");
			parseExp(e->u.whilee.body);
			printf(")");
			break;
		default:
			assert(0);
	}
}

void parseTy(A_ty t) {
	switch (t->kind) {
		case A_nameTy:
			printf("%s ", S_name(t->u.name));
			break;
		case A_recordTy: {
			A_fieldList fl = t->u.record;
			printf("{ ");
			while (fl) {
				printf("%s:%s", S_name(fl->head->name), S_name(fl->head->typ));
				if (fl->tail) {
					printf(", ");
				}
				fl = fl->tail;
			}
			printf(" }");
			break;
		}
		case A_arrayTy:
			printf("array of %s", S_name(t->u.array));
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
