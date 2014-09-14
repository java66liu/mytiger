%{
/****************************************************************************
myparser.y
ParserWizard generated YACC file.

Date: 2007Äê9ÔÂ25ÈÕ
****************************************************************************/
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

/////////////////////////////////////////////////////////////////////////////
// declarations section

// place any declarations here
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

%token COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK
%token LBRACE RBRACE DOT PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE 
%token AND OR ASSIGN ARRAY IF THEN ELSE WHILE FOR TO DO LET IN END OF
%token BREAK NIL FUNCTION VAR TYPE UMINUS

%type <sval> ID1
%type <exp> exp prog
%type <var> L_value L_value2
%type <expList> expseq explist
%type <efieldList> assignlist
%type <decList> decs
%type <dec> dec vardec 
%type <ty> ty
%type <namety> tydec
%type <fieldList> tyfields
%type <fundec> fundec

%start prog

%right ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%left UMINUS


%%

/////////////////////////////////////////////////////////////////////////////
// rules section

// place your YACC rules here (there must be at least one)
prog : exp { absyn_root = $1; }
     ;
exp : L_value { $$ = A_VarExp($1->pos, $1); }
    | NIL { $$ = A_NilExp(EM_tokPos); }
    | LPAREN expseq RPAREN { $$ = A_SeqExp($2->head->pos, $2); }
    | LPAREN RPAREN { $$ = A_SeqExp(EM_tokPos - 2, NULL); }
    | INT { $$ = A_IntExp(EM_tokPos, yylval.ival); }
    | STRING { $$ = A_StringExp(EM_tokPos, String(yylval.sval)); }
    | MINUS exp %prec UMINUS { $$ = A_OpExp($2->pos, A_minusOp, A_IntExp($2->pos, 0), $2); }
    | ID1 LPAREN RPAREN { $$ = A_CallExp(EM_tokPos, S_Symbol($1), NULL); }
    | ID1 LPAREN explist RPAREN { $$ = A_CallExp(EM_tokPos, S_Symbol($1), $3); }
    | exp PLUS exp { $$ = A_OpExp($1->pos, A_plusOp, $1, $3); }
    | exp MINUS exp { $$ = A_OpExp($1->pos, A_minusOp, $1, $3); }
    | exp TIMES exp { $$ = A_OpExp($1->pos, A_timesOp, $1, $3); }
    | exp DIVIDE exp { $$ = A_OpExp($1->pos, A_divideOp, $1, $3); }
    | exp EQ exp { $$ = A_OpExp($1->pos, A_eqOp, $1, $3); }
    | exp NEQ exp { $$ = A_OpExp($1->pos, A_neqOp, $1, $3); }
    | exp LT exp { $$ = A_OpExp($1->pos, A_ltOp, $1, $3); }
    | exp LE exp { $$ = A_OpExp($1->pos, A_leOp, $1, $3); }
    | exp GT exp { $$ = A_OpExp($1->pos, A_gtOp, $1, $3); }
    | exp GE exp { $$ = A_OpExp($1->pos, A_geOp, $1, $3); }
    | exp AND exp { $$ = A_IfExp($1->pos, $1, $3, A_IntExp($1->pos, 0)); }
    | exp OR exp { $$ = A_IfExp($1->pos, $1, A_IntExp($1->pos, 1), $3); }
    | ID1 LBRACE RBRACE { $$ = A_RecordExp(EM_tokPos, S_Symbol($1), NULL); }
    | ID1 LBRACE assignlist RBRACE { $$ = A_RecordExp(EM_tokPos, S_Symbol($1), $3); }
    | ID1 LBRACK exp RBRACK OF exp { $$ = A_ArrayExp($3->pos, S_Symbol($1), $3, $6); }
    | L_value ASSIGN exp { $$ = A_AssignExp($1->pos, $1, $3); }
    | IF exp THEN exp ELSE exp { $$ = A_IfExp($2->pos, $2, $4, $6); }
    | IF exp THEN exp { $$ = A_IfExp($2->pos, $2, $4, NULL); }
    | WHILE exp DO exp { $$ = A_WhileExp($2->pos, $2, $4); }
    | FOR ID1 ASSIGN exp TO exp DO exp { $$ = A_ForExp($4->pos, S_Symbol($2), $4, $6, $8); } 
    | BREAK { $$ = A_BreakExp(EM_tokPos); }
    | LET decs IN expseq END { $$ = A_LetExp($2->head->pos, $2, A_SeqExp($4->head->pos, $4)); }
    | LET decs IN END { $$ = A_LetExp($2->head->pos, $2, NULL); }
    | LET IN expseq END { $$ = A_LetExp($3->head->pos, NULL, A_SeqExp($3->head->pos, $3)); }
    | LET IN END { $$ = A_LetExp(EM_tokPos, NULL, NULL); }
    ;
L_value : ID1 { $$ = A_SimpleVar(EM_tokPos, S_Symbol($1)); }
        | L_value2 { $$ = $1; }
        ;
L_value2 : ID1 LBRACK exp RBRACK { $$ = A_SubscriptVar(EM_tokPos, A_SimpleVar(EM_tokPos, S_Symbol($1)), $3); }
         | ID1 DOT ID1 { $$ = A_FieldVar(EM_tokPos, A_SimpleVar(EM_tokPos, S_Symbol($1)), S_Symbol($3)); }
         | L_value2 DOT ID1 { $$ = A_FieldVar(EM_tokPos, $1, S_Symbol($3)); }
         | L_value2 LBRACK exp RBRACK { $$ = A_SubscriptVar(EM_tokPos, $1, $3); }
        ;
expseq : exp { $$ = A_ExpList($1, NULL); }
       | expseq SEMICOLON exp { A_expList ptr;
                                ptr = $1;
                                while(ptr->tail != NULL) { ptr = ptr->tail; }
                                ptr->tail = A_ExpList($3, NULL);
                                $$ = $1;
                               }
                                  
       ;
explist : exp { $$ = A_ExpList($1, NULL); }
        | explist COMMA exp { A_expList ptr;
                              ptr = $1;
                              while(ptr->tail != NULL) { ptr = ptr->tail; }
                              ptr->tail = A_ExpList($3, NULL); 
                              $$ = $1;
                             }
        ;

assignlist : ID1 EQ exp { $$ = A_EfieldList(A_Efield(S_Symbol($1), $3), NULL); }
           | assignlist COMMA ID1 EQ exp { A_efieldList ptr;
                                          ptr = $1; 
                                          while(ptr->tail != NULL) { ptr = ptr->tail; }
                                          ptr->tail = A_EfieldList(A_Efield(S_Symbol($3), $5), NULL); 
                                          $$ = $1;
                                         }
           ;
decs : dec { $$ = A_DecList($1, NULL); }
     | decs dec { A_decList ptr;
                  ptr = $1; 
                  while(ptr->tail != NULL) { ptr = ptr->tail; }
                  if($2->kind == A_varDec || ptr->head->kind == A_varDec) {
                    ptr->tail = A_DecList($2, NULL);
                  }
                  else if($2->kind != ptr->head->kind) {
                    ptr->tail = A_DecList($2, NULL); 
                  }
                  else if($2->kind == A_functionDec) {
                    A_fundecList ptr2;
                    ptr2 = ptr->head->u.function;
                    while(ptr2->tail != NULL) { ptr2 = ptr2->tail; }
                    ptr2->tail = A_FundecList($2->u.function->head, NULL);
                  }
                  else {
                    A_nametyList ptr2;
                    ptr2 = ptr->head->u.type;
                    while(ptr2->tail != NULL) { ptr2 = ptr2->tail; }
                    ptr2->tail = A_NametyList($2->u.type->head, NULL);
                  }
                  $$ = $1;
                }
                    
     ;
dec : tydec { $$ = A_TypeDec($1->ty->pos, A_NametyList($1, NULL)); }
    | vardec { $$ = $1; }
    | fundec { $$ = A_FunctionDec($1->pos, A_FundecList($1, NULL)); }
    ;
tydec : TYPE ID1 EQ ty { $$ = A_Namety(S_Symbol($2), $4); }
      ;
ty : ID1 { $$ = A_NameTy(EM_tokPos, S_Symbol($1)); }
   | LBRACE RBRACE { $$ = A_RecordTy(EM_tokPos, NULL); }
   | LBRACE tyfields RBRACE { $$ = A_RecordTy(EM_tokPos, $2); }
   | ARRAY OF ID1 { $$ = A_ArrayTy(EM_tokPos, S_Symbol($3)); }
   ;
tyfields : ID1 COLON ID1 { $$ = A_FieldList(A_Field(EM_tokPos, S_Symbol($1), S_Symbol($3)), NULL); }
         | tyfields COMMA ID1 COLON ID1 { A_fieldList ptr;
                                        ptr = $1;
                                        while(ptr->tail != NULL) { ptr = ptr->tail; }
                                        ptr->tail = A_FieldList(A_Field(EM_tokPos, 
                                                                 S_Symbol($3), S_Symbol($5)), NULL);
                                        $$ = $1;
                                       }
         ;
vardec : VAR ID1 ASSIGN exp { $$ = A_VarDec($4->pos, S_Symbol($2), NULL, $4); }
       | VAR ID1 COLON ID1 ASSIGN exp { $$ = A_VarDec($6->pos, S_Symbol($2), S_Symbol($4), $6); }
       ;
fundec : FUNCTION ID1 LPAREN RPAREN EQ exp { $$ = A_Fundec($6->pos, S_Symbol($2), NULL, NULL, $6); }
       | FUNCTION ID1 LPAREN RPAREN COLON ID1 EQ exp { $$ = A_Fundec($8->pos, S_Symbol($2), NULL, S_Symbol($6), $8); }
       | FUNCTION ID1 LPAREN tyfields RPAREN EQ exp { $$ = A_Fundec($7->pos, S_Symbol($2), $4, NULL, $7); }
       | FUNCTION ID1 LPAREN tyfields RPAREN COLON ID1 EQ exp { $$ = A_Fundec($9->pos, S_Symbol($2), $4, S_Symbol($7), $9); }
       ;

ID1 : ID { $$ = String(yylval.sval); }
    ;
%%

/////////////////////////////////////////////////////////////////////////////
// programs section

int yygettoken(void)  
{
	// place your token retrieving code here
	return yylex();
	return 0;
}

YYSTYPE yylval; 
/*int main(void)
{
	return yyparse();
}*/

