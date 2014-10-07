%{
#include <stdio.h>
#include "util.h"
#include "errormsg.h"
#include "symbol.h"
#include "absyn.h"

int yylex(void); /* function prototype */

A_exp absyn_root;

void yyerror(char *s)
{
	EM_error(EM_tokPos, "%s", s);
}
%}

%union {
	int pos;
	int ival;
	string sval;
	A_var var;
	A_exp exp;
	A_dec dec;
	A_ty ty;
	A_decList decList;
	A_expList expList;
	A_field field;
	A_fieldList fieldList;
	A_fundec fundec;
	A_fundecList fundecList;
	A_namety namety;
	A_nametyList nametyList;
	A_efieldList efieldList;
	A_efield efield;
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

%type <exp> expr program
%type <var> lvalue
%type <expList> exprSeq exprList
%type <efieldList> fieldList
%type <efield> field
%type <decList> declList
%type <dec> decl varDecl
%type <ty> type
%type <namety> typeDecl
%type <fieldList> typeFields
%type <fundec> funcDecl
%type <sval> id2

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
:	expr	{ absyn_root = $1; }

id2
:	ID { $$ = String(yylval.sval); }

expr
:	STRING	{ $$ = A_StringExp(EM_tokPos, String(yylval.sval)); }
|	INT	{ $$ = A_IntExp(EM_tokPos, yylval.ival); }
|	NIL	{ $$ = A_NilExp(EM_tokPos); }
|	lvalue	{ $$ = A_VarExp($1->pos, $1); }
|	MINUS expr %prec UMINUS	{ $$ = A_OpExp($2->pos, A_minusOp, A_IntExp($2->pos, 0), $2); }

|	expr PLUS expr	{ $$ = A_OpExp($1->pos, A_plusOp, $1, $3); }
|	expr MINUS expr	{ $$ = A_OpExp($1->pos, A_minusOp, $1, $3); }
|	expr TIMES expr	{ $$ = A_OpExp($1->pos, A_timesOp, $1, $3); }
|	expr DIVIDE expr	{ $$ = A_OpExp($1->pos, A_divideOp, $1, $3); }

|	expr OR expr	{ $$ = A_IfExp($1->pos, $1, A_IntExp($1->pos, 1), $3); }
|	expr AND expr	{ $$ = A_IfExp($1->pos, $1, $3, A_IntExp($1->pos, 0)); }

|	expr EQ expr	{ $$ = A_OpExp($1->pos, A_eqOp, $1, $3); }
|       expr NEQ expr	{ $$ = A_OpExp($1->pos, A_neqOp, $1, $3); }
|       expr LT expr	{ $$ = A_OpExp($1->pos, A_ltOp, $1, $3); }
|       expr LE expr	{ $$ = A_OpExp($1->pos, A_leOp, $1, $3); }
|       expr GT expr	{ $$ = A_OpExp($1->pos, A_gtOp, $1, $3); }
|       expr GE expr	{ $$ = A_OpExp($1->pos, A_geOp, $1, $3); }

|	lvalue ASSIGN expr	{ $$ = A_AssignExp($1->pos, $1, $3); }

|	id2 LPAREN exprList RPAREN	{ $$ = A_CallExp(EM_tokPos, S_Symbol($1), $3); }
|	id2 LPAREN RPAREN	{ $$ = A_CallExp(EM_tokPos, S_Symbol($1), NULL); }

|	LPAREN exprSeq RPAREN	{ $$ = A_SeqExp($2->head->pos, $2); }
|	LPAREN RPAREN	{ $$ = A_SeqExp(EM_tokPos - 2, NULL); }

|	id2 LBRACE fieldList RBRACE	{ $$ = A_RecordExp(EM_tokPos, S_Symbol($1), $3); }
|	id2 LBRACE RBRACE	{ $$ = A_RecordExp(EM_tokPos, S_Symbol($1), NULL); }

|	id2 LBRACK expr RBRACK OF expr	{ $$ = A_ArrayExp($3->pos, S_Symbol($1), $3, $6); }

|	IF expr THEN expr	{ $$ = A_IfExp($2->pos, $2, $4, NULL); }
|	IF expr THEN expr ELSE expr	{ $$ = A_IfExp($2->pos, $2, $4, $6); }

|	WHILE expr DO expr	{ $$ = A_WhileExp($2->pos, $2, $4); }

|	FOR id2 ASSIGN expr TO expr DO expr	{ $$ = A_ForExp($4->pos, S_Symbol($2), $4, $6, $8); } 

|	BREAK	{ $$ = A_BreakExp(EM_tokPos); }

|	LET declList IN exprSeq END	{ $$ = A_LetExp($2->head->pos, $2, A_SeqExp($4->head->pos, $4)); }
|	LET declList IN END	{ $$ = A_LetExp($2->head->pos, $2, NULL); }

exprSeq
:	expr	{ $$ = A_ExpList($1, NULL); }
|	exprSeq SEMICOLON expr	{
					A_expList ptr;
					ptr = $1;
					while (ptr->tail != NULL) { ptr = ptr->tail; }
					ptr->tail = A_ExpList($3, NULL);
					$$ = $1;
				}

exprList
:	expr	{ $$ = A_ExpList($1, NULL); }
|	exprList COMMA expr	{
					A_expList ptr;
					ptr = $1;
					while (ptr->tail != NULL) { ptr = ptr->tail; }
					ptr->tail = A_ExpList($3, NULL); 
					$$ = $1;
				}

field
:	id2 EQ expr      { $$ = A_Efield(S_Symbol($1), $3); }

fieldList
:	field	{ $$ = A_EfieldList($1, NULL); }
|	fieldList COMMA field	{
					A_efieldList ptr;
					ptr = $1; 
					while (ptr->tail != NULL) { ptr = ptr->tail; }
					ptr->tail = A_EfieldList($3, NULL); 
					$$ = $1;
				}

lvalue
:	id2	{ $$ = A_SimpleVar(EM_tokPos, S_Symbol($1)); }
|	lvalue LBRACK expr RBRACK	{ $$ = A_SubscriptVar(EM_tokPos, $1, $3); }
|	id2 LBRACK expr RBRACK	{ $$ = A_SubscriptVar(EM_tokPos, A_SimpleVar(EM_tokPos, S_Symbol($1)), $3); }
|	lvalue DOT id2	{ $$ = A_FieldVar(EM_tokPos, $1, S_Symbol($3)); }

declList
:	decl	{ $$ = A_DecList($1, NULL); }
|	declList decl	{
				A_decList ptr;
				ptr = $1; 
				while (ptr->tail != NULL) { ptr = ptr->tail; }
				if ($2->kind == A_varDec || ptr->head->kind == A_varDec) {
					ptr->tail = A_DecList($2, NULL);
				}
				else if ($2->kind != ptr->head->kind) {
					ptr->tail = A_DecList($2, NULL); 
				}
				else if ($2->kind == A_functionDec) {
					A_fundecList ptr2;
					ptr2 = ptr->head->u.function;
					while (ptr2->tail != NULL) { ptr2 = ptr2->tail; }
					ptr2->tail = A_FundecList($2->u.function->head, NULL);
				}
				else {
					A_nametyList ptr2;
					ptr2 = ptr->head->u.type;
					while (ptr2->tail != NULL) { ptr2 = ptr2->tail; }
					ptr2->tail = A_NametyList($2->u.type->head, NULL);
				}
				$$ = $1;
			}

decl
:	typeDecl { $$ = A_TypeDec($1->ty->pos, A_NametyList($1, NULL)); }
|	varDecl	{ $$ = $1; }
|	funcDecl { $$ = A_FunctionDec($1->pos, A_FundecList($1, NULL)); }

typeDecl
:	TYPE id2 EQ type	{ $$ = A_Namety(S_Symbol($2), $4); }

type
:	id2	{ $$ = A_NameTy(EM_tokPos, S_Symbol($1)); }
|	LBRACE typeFields RBRACE	{ $$ = A_RecordTy(EM_tokPos, $2); }
|	LBRACE RBRACE	{ $$ = A_RecordTy(EM_tokPos, NULL); }
|	ARRAY OF id2	{ $$ = A_ArrayTy(EM_tokPos, S_Symbol($3)); }

typeFields
:	id2 COLON id2	{ $$ = A_FieldList(A_Field(EM_tokPos, S_Symbol($1), S_Symbol($3)), NULL); }
|	typeFields COMMA id2 COLON id2	{
						A_fieldList ptr;
						ptr = $1;
						while (ptr->tail != NULL) { ptr = ptr->tail; }
						ptr->tail = A_FieldList(A_Field(EM_tokPos, S_Symbol($3), S_Symbol($5)), NULL);
						$$ = $1;
					}

varDecl
:	VAR id2 ASSIGN expr	{ $$ = A_VarDec($4->pos, S_Symbol($2), NULL, $4); }
|	VAR id2 COLON id2 ASSIGN expr	{ $$ = A_VarDec($6->pos, S_Symbol($2), S_Symbol($4), $6); }

funcDecl
:	FUNCTION id2 LPAREN typeFields RPAREN EQ expr	{ $$ = A_Fundec($7->pos, S_Symbol($2), $4, NULL, $7); }
|	FUNCTION id2 LPAREN RPAREN EQ expr	{ $$ = A_Fundec($6->pos, S_Symbol($2), NULL, NULL, $6); }
|	FUNCTION id2 LPAREN typeFields RPAREN COLON id2 EQ expr	{ $$ = A_Fundec($9->pos, S_Symbol($2), $4, S_Symbol($7), $9); }
|	FUNCTION id2 LPAREN RPAREN COLON id2 EQ expr	{ $$ = A_Fundec($8->pos, S_Symbol($2), NULL, S_Symbol($6), $8); }
