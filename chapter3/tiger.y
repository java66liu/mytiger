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
: decs
| exp

//----decs----
decs
: decs dec
| dec
	
	dec
	: tydec
	| vardec
	| fundec

		tydec
		: TYPE ID EQ ty
		| LBRACK tyfields RBRACK
		| ARRAY OF ID

			ty
			: ID
			| ARRAY OF ID
			| LBRACE tyfields RBRACE

			tyfields
			: tyfields COMMA tyfield
			| tyfield

				tyfield
				: ID COLON ID
				| empty

		vardec
		: VAR ID ASSIGN exp
		| VAR ID COLON ID ASSIGN exp

		fundec
		: FUNCTION ID LPAREN tyfields RPAREN EQ exp
		| FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp
		
//----exp----

exp
: lvalue
| no_value_exp
| nil
| list
| int
| string
| function
| operation
| compare
| new
| assign
| if_then
| if_then_else
| while_do
| for_to_do
| break
| let_in_end
| parentheses

	lvalue
	: ID
	| lvalue DOT ID
	| lvalue LBRACK exp RBRACK

	no_value_exp
	: LPAREN RPAREN | LBRACK RBRACK | LBRACE RBRACE
	| empty
	
	nil
	: NIL

	list
	: LPAREN list_exps RPAREN
		
		list_exps
		: list_exps SEMICOLON exp
		| exp SEMICOLON exp

	int
	: INT
	| MINUS int

	string
	: STRING

	function
	: ID LPAREN RPAREN
	| ID LPAREN function_exps RPAREN
	
		function_exps
		: function_exps COMMA exp
		| exp

	operation
	: exp_operation
	| bool_operation
	
		exp_operation
		: exp exp_op exp
		
			exp_op
			: PLUS | MINUS | TIMES | DIVIDE
			
		bool_operation
		: exp bool_op exp
			
			bool_op
			: AND | OR
	
	compare
	: exp_compare
	| string_compare
	
		exp_compare
		: exp compare_op exp
		
		string_compare
		: STRING compare_op STRING
		
			compare_op
			: EQ | NEQ | LT | LE | GT | GE
			
	new
	: type_new
	| array_new
	
		type_new
		: ID LBRACE type_new_exps RBRACE
		
			type_new_exps
			: type_new_exps COMMA ID EQ exp
			| ID EQ exp
			
		array_new
		: ID LBRACK exp RBRACK OF exp

	assign
	: lvalue ASSIGN exp

	if_then
	: IF exp THEN exp

	if_then_else
	: IF exp THEN exp ELSE exp

	while_do
	: WHILE exp DO exp

	for_to_do
	: FOR exp TO exp DO exp

	break
	: BREAK;
	
	let_in_end
	: LET decs IN expseq END
	| LET decs IN END

		expseq
		: expseq SEMICOLON exp
		| exp

	parentheses
	: LPAREN exp RPAREN

//---- ----
empty
:


















