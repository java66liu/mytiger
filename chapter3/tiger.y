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

%nonassoc THEN DO TYPE FUNCTION ID
%nonassoc ASSIGN LBRACK ELSE OF
%left OR
%left AND
%nonassoc EQ NEQ LE LT GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%left UMINUS

%start program
%%

program
:	expr

expr
:	STRING
|	INT
|	NIL
|	lvalue
|	MINUS expr %prec UMINUS

|	expr PLUS expr
|	expr MINUS expr
|	expr TIMES expr
|	expr DIVIDE expr

|	expr OR expr
|	expr AND expr

|	expr EQ expr
|       expr NEQ expr
|       expr LT expr
|       expr LE expr
|       expr GT expr
|       expr GE expr

|	lvalue ASSIGN expr

|	ID LPAREN exprList RPAREN
|	ID LPAREN RPAREN

|	LPAREN exprSeq RPAREN
|	LPAREN RPAREN

|	ID LBRACE fieldList RBRACE
|	ID LBRACE RBRACE

|	ID LBRACK expr RBRACK OF expr

|	IF expr THEN expr
|	IF expr THEN expr ELSE expr

|	WHILE expr DO expr

|	FOR ID ASSIGN expr TO expr DO expr

|	BREAK

|	LET declList IN exprSeq END
|	LET declList IN END

exprSeq
:	expr
|	exprSeq SEMICOLON expr

exprList
:	expr
|	exprList COMMA expr

field
:	ID EQ expr

fieldList
:	field
|	fieldList COMMA field

lvalue
:	ID
|	lvalue LBRACK expr RBRACK
|	ID LBRACK expr RBRACK
|	lvalue DOT ID

declList
:	decl
|	declList decl

decl
:	typeDecl
|	varDecl
|	funcDecl

typeDecl
:	TYPE ID EQ type

type
:	ID
|	LBRACE typeFields RBRACE
|	LBRACE RBRACE
|	ARRAY OF ID

typeFields
:	ID COLON ID
|	typeFields COMMA ID COLON ID

varDecl
:	VAR ID ASSIGN expr
|	VAR ID COLON ID ASSIGN expr

funcDecl
:	FUNCTION ID LPAREN typeFields RPAREN EQ expr
|	FUNCTION ID LPAREN RPAREN EQ expr
|	FUNCTION ID LPAREN typeFields RPAREN COLON ID EQ expr
|	FUNCTION ID LPAREN RPAREN COLON ID EQ expr




