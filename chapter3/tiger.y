%{
#include <stdio.h>
#include "util.h"
#include "errormsg.h"

int yylex(void); /* function prototype */

void yyerror(char *s)
{
 EM_error(EM_tokPos, "%s", s);
}
%}

%union {
	int pos;
	int ival;
	string sval;
}

%token <sval> ID STRING
%token <ival> INT

%token 
  COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK 
  LBRACE RBRACE DOT 
  PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE
  AND OR ASSIGN
  ARRAY IF THEN ELSE WHILE FOR TO DO LET IN END OF 
  BREAK NIL
  FUNCTION VAR TYPE 

%start program
%%

program
: exps

exps
: exps exp
| exp

exp
: S

S
: M
| U

M
: IF E THEN M ELSE M
| other 

U
: IF E THEN S
| IF E THEN M ELSE U

other
: INT

E
: INT

