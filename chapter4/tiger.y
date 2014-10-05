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
	EM_error(EM_tokPos, "|%s|", s);
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

expr
:	STRING	{ $$ = A_StringExp(EM_tokPos, String(yylval.sval)); printf("STRING\n");}
|	INT	{ $$ = A_IntExp(EM_tokPos, yylval.ival); printf("INT\n");}
|	NIL	{ $$ = A_NilExp(EM_tokPos); printf("NIL\n");}
|	lvalue	{ $$ = A_VarExp($1->pos, $1); printf("lvalue\n");}
|	MINUS expr %prec UMINUS	{ $$ = A_OpExp($2->pos, A_minusOp, A_IntExp($2->pos, 0), $2); printf("MINUS expr\n");}

|	expr PLUS expr	{ $$ = A_OpExp($1->pos, A_plusOp, $1, $3); printf("PLUS\n");}
|	expr MINUS expr	{ $$ = A_OpExp($1->pos, A_minusOp, $1, $3); printf("MINUS\n");}
|	expr TIMES expr	{ $$ = A_OpExp($1->pos, A_timesOp, $1, $3); printf("TIMES\n");}
|	expr DIVIDE expr	{ $$ = A_OpExp($1->pos, A_divideOp, $1, $3); printf("DIVIDE\n");}

|	expr OR expr	{ $$ = A_IfExp($1->pos, $1, A_IntExp($1->pos, 1), $3); printf("OR\n");}
|	expr AND expr	{ $$ = A_IfExp($1->pos, $1, $3, A_IntExp($1->pos, 0)); printf("AND\n");}

|	expr EQ expr	{ $$ = A_OpExp($1->pos, A_eqOp, $1, $3); printf("EQ\n");}
|   expr NEQ expr	{ $$ = A_OpExp($1->pos, A_neqOp, $1, $3); printf("NEQ\n");}
|   expr LT expr	{ $$ = A_OpExp($1->pos, A_ltOp, $1, $3); printf("LT\n");}
|   expr LE expr	{ $$ = A_OpExp($1->pos, A_leOp, $1, $3); printf("LE\n");}
|   expr GT expr	{ $$ = A_OpExp($1->pos, A_gtOp, $1, $3); printf("GT\n");}
|   expr GE expr	{ $$ = A_OpExp($1->pos, A_geOp, $1, $3); printf("GE\n");}

|	lvalue ASSIGN expr	{ $$ = A_AssignExp($1->pos, $1, $3); printf("lvalue ASSIGN expr\n");}

|	ID LPAREN exprList RPAREN	{ $$ = A_CallExp(EM_tokPos, S_Symbol($1), $3); printf("|%s| (exprList)\n", $1);}
|	ID LPAREN RPAREN	{ $$ = A_CallExp(EM_tokPos, S_Symbol($1), NULL); printf("|%s| ()\n", $1);}

|	LPAREN exprSeq RPAREN	{ $$ = A_SeqExp($2->head->pos, $2); printf("(exprSeq)\n");}
|	LPAREN RPAREN	{ $$ = A_SeqExp(EM_tokPos - 2, NULL); printf("()\n");}

|	ID LBRACE fieldList RBRACE	{ $$ = A_RecordExp(EM_tokPos, S_Symbol($1), $3); printf("|%s| {fieldList}\n", $1);}
|	ID LBRACE RBRACE	{ $$ = A_RecordExp(EM_tokPos, S_Symbol($1), NULL); printf("|%s| {}\n", $1);}

|	ID LBRACK expr RBRACK OF expr	{ $$ = A_ArrayExp($3->pos, S_Symbol($1), $3, $6); printf("|%s| [expr] OF expr\n", $1);}

|	IF expr THEN expr	{ $$ = A_IfExp($2->pos, $2, $4, NULL); printf("IF THEN\n");}
|	IF expr THEN expr ELSE expr	{ $$ = A_IfExp($2->pos, $2, $4, $6); printf("IF THEN ELSE\n");}

|	WHILE expr DO expr	{ $$ = A_WhileExp($2->pos, $2, $4); printf("WHILE DO\n");}

|	FOR ID ASSIGN expr TO expr DO expr	{ $$ = A_ForExp($4->pos, S_Symbol($2), $4, $6, $8); printf("FOR |%s| TO DO\n", $2);}

|	BREAK	{ $$ = A_BreakExp(EM_tokPos); printf("BREAK\n");}

|	LET declList IN exprSeq END	{ $$ = A_LetExp($2->head->pos, $2, A_SeqExp($4->head->pos, $4)); printf("LET IN END\n");}
|	LET declList IN END	{ $$ = A_LetExp($2->head->pos, $2, NULL); printf("LET IN\n");}

exprSeq
:	expr	{ $$ = A_ExpList($1, NULL); printf("expr\n");}
|	exprSeq SEMICOLON expr	{
					A_expList ptr;
					ptr = $1;
					while (ptr->tail != NULL) { ptr = ptr->tail; }
					ptr->tail = A_ExpList($3, NULL);
					$$ = $1;
					printf("exprSeq\n");
				}

exprList
:	expr	{ $$ = A_ExpList($1, NULL); printf("expr\n");}
|	exprList COMMA expr	{
					A_expList ptr;
					ptr = $1;
					while (ptr->tail != NULL) { ptr = ptr->tail; }
					ptr->tail = A_ExpList($3, NULL); 
					$$ = $1;
					printf("exprList\n");
				}

field
:	ID EQ expr      { $$ = A_Efield(S_Symbol($1), $3); printf("|%s| =\n", $1);}

fieldList
:	field	{ $$ = A_EfieldList($1, NULL); printf("field\n");}
|	fieldList COMMA field	{
					A_efieldList ptr;
					ptr = $1; 
					while (ptr->tail != NULL) { ptr = ptr->tail; }
					ptr->tail = A_EfieldList($3, NULL); 
					$$ = $1;
					printf("fieldList\n");
				}

lvalue
:	ID	{ $$ = A_SimpleVar(EM_tokPos, S_Symbol($1)); printf("|%s|\n", $1);}
|	lvalue LBRACK expr RBRACK	{ $$ = A_SubscriptVar(EM_tokPos, $1, $3); printf("lvalue[expr]\n");}
|	ID LBRACK expr RBRACK	{ $$ = A_SubscriptVar(EM_tokPos, A_SimpleVar(EM_tokPos, S_Symbol($1)), $3); printf("|%s|[expr]\n", $1);}
|	lvalue DOT ID	{ $$ = A_FieldVar(EM_tokPos, $1, S_Symbol($3)); printf("lvalue.|%s|\n", $3);}

declList
:	decl	{ $$ = A_DecList($1, NULL); printf("decl\n");}
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
				printf("\n");
			}

decl
:	typeDecl	{ $$ = A_TypeDec($1->ty->pos, A_NametyList($1, NULL)); printf("typeDecl\n");}
|	varDecl	{ $$ = $1; printf("varDecl\n");}
|	funcDecl	{ $$ = A_FunctionDec($1->pos, A_FundecList($1, NULL)); printf("funcDecl\n");}

typeDecl
:	TYPE ID EQ type	{ $$ = A_Namety(S_Symbol($2), $4); printf("TYPE |%s| = type\n", $2);}

type
:	ID	{ $$ = A_NameTy(EM_tokPos, S_Symbol($1)); printf("type_ID|%s|\n", $1);}
|	LBRACE typeFields RBRACE	{ $$ = A_RecordTy(EM_tokPos, $2); printf("{typefields}\n");}
|	LBRACE RBRACE	{ $$ = A_RecordTy(EM_tokPos, NULL); printf("{}\n");}
|	ARRAY OF ID	{ $$ = A_ArrayTy(EM_tokPos, S_Symbol($3)); printf("ARRAY OF |%s|\n", $3);}

typeFields
:	ID COLON ID	{ $$ = A_FieldList(A_Field(EM_tokPos, S_Symbol($1), S_Symbol($3)), NULL); printf("|%s| : |%s|\n", $1, $3);}
|	typeFields COMMA ID COLON ID	{
						A_fieldList ptr;
						ptr = $1;
						while (ptr->tail != NULL) { ptr = ptr->tail; }
						ptr->tail = A_FieldList(A_Field(EM_tokPos, S_Symbol($3), S_Symbol($5)), NULL);
						$$ = $1;
						printf("\n");
					}

varDecl
:	VAR ID ASSIGN expr	{ $$ = A_VarDec($4->pos, S_Symbol($2), NULL, $4); printf("VAR |%s| = expr\n", $2);}
|	VAR ID COLON ID ASSIGN expr	{ $$ = A_VarDec($6->pos, S_Symbol($2), S_Symbol($4), $6); printf("VAR |%s| : |%s| = expr\n", $2, $4);}

funcDecl
:	FUNCTION ID LPAREN typeFields RPAREN EQ expr	{ $$ = A_Fundec($7->pos, S_Symbol($2), $4, NULL, $7); printf("FUNCTION |%s|\n", $2);}
|	FUNCTION ID LPAREN RPAREN EQ expr	{ $$ = A_Fundec($6->pos, S_Symbol($2), NULL, NULL, $6); printf("\n");}
|	FUNCTION ID LPAREN typeFields RPAREN COLON ID EQ expr	{ $$ = A_Fundec($9->pos, S_Symbol($2), $4, S_Symbol($7), $9); printf("\n");}
|	FUNCTION ID LPAREN RPAREN COLON ID EQ expr	{ $$ = A_Fundec($8->pos, S_Symbol($2), NULL, S_Symbol($6), $8); printf("\n");}
