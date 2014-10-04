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

int main(int argc, char **argv) {
	A_exp main;
	if (argc!=2) {
		fprintf(stderr,"usage: a.out filename\n");
		exit(1);
	}
 	main = parse(argv[1]);
	switch (main->kind) {
		case A_letExp:
			printf("let\n");
			break;
		default:
			printf("error\n");
	}
	return 0;
}
