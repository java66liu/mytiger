%{
#include <string.h>
#include "util.h"
#include "tokens.h"
#include "errormsg.h"

int charPos = 1;
int commentCount;
string str;
char ddd_c;
int ddd_i;

int yywrap(void)
{
	charPos=1;
	return 1;
}


void adjust(void)
{
	EM_tokPos=charPos;
	charPos+=yyleng;
}

%}

LineTerminator	\r|\n|\r\n
WhiteSpace	LineTerminator|[ \t\f]
Identifier	[_a-zA-Z]([_a-zA-Z]|[0-9])*
DecInteger	[0-9][0-9]*

%state COMMENT
%state INSTRING

%%

<INITIAL>
{
	\"	{BEGIN INSTRING; str = String("");}
	"/*"	{commentCount=1; BEGIN COMMENT;}

	"while"	{adjust(); return WHILE;}
	"for"	{adjust(); return FOR;}
	"to"	{adjust(); return TO;}
	"break"	{adjust(); return BREAK;}
	"let"	{adjust(); return LET;}
	"in"	{adjust(); return IN;}
	"end"	{adjust(); return END;}
	"function"	{adjust(); return FUNCTION;}
	"var"	{adjust(); return VAR;}
	"type"	{adjust(); return TYPE;}
	"array"	{adjust(); return ARRAY;}
	"if"	{adjust(); return IF;}
	"then"	{adjust(); return THEN;}
	"else"	{adjust(); return ELSE;}
	"do"	{adjust(); return DO;}
	"of"	{adjust(); return OF;}
	"nil"	{adjust(); return NIL;}

	","	{adjust(); return COMMA;}
	":"	{adjust(); return COLON;}
	";"	{adjust(); return SEMICOLON;}
	"("	{adjust(); return LPAREN;}
	")"	{adjust(); return RPAREN;}
	"["	{adjust(); return LBRACK;}
	"]"	{adjust(); return RBRACK;}
	"{"	{adjust(); return LBRACE;}
	"}"	{adjust(); return RBRACE;}
	"."	{adjust(); return DOT;}
	"+"	{adjust(); return PLUS;}
	"-"	{adjust(); return MINUS;}
	"*"	{adjust(); return TIMES;}
	"/"	{adjust(); return DIVIDE;}
	"="	{adjust(); return EQ;}
	"<>"	{adjust(); return NEQ;}
	"<"	{adjust(); return LT;}
	"<="	{adjust(); return LE;}
	">"	{adjust(); return GT;}
	">="	{adjust(); return GE;}
	"&"	{adjust(); return AND;}
	"|"	{adjust(); return OR;}
	":="	{adjust(); return ASSIGN;}

	{Identifier}	{adjust(); yylval.sval=yytext; return ID;}
	{LineTerminator}	{EM_newline();}
	{WhiteSpace}	{}
	{DecInteger}	{adjust(); yylval.ival=atoi(yytext); return INT;}

	.	{EM_error(charPos, "Illegal character < ", yytext, " >");}
}

<INSTRING>
{
	\"	{BEGIN INITIAL; yylval.sval = str; return STRING;}
	{LineTerminator}	{EM_error(charPos, "Unterminated string at end of line");}
	\\n	{adjust(); str = strcpy(str + 1 , "\n");}
	\\t	{adjust(); str = strcpy(str + 1 , "\t");}
	\\\"	{adjust(); str = strcpy(str + 1 , "\"");}
	\\\\	{adjust(); str = strcpy(str + 1 , "\\");}
        \\[0-9][0-9][0-9]       {adjust(); ddd_i = atoi(yytext + 1); if (ddd_i>255) {EM_error(charPos, "Illegal string with \\ddd");} else {ddd_c = (char) ddd_i; str = strcpy(str + 1, &ddd_c);}}
        (\\.|[^\\"])*   {adjust(); str = strcpy(str + yyleng, yytext); adjust();}
	\\WhiteSpace+\\	{}
	.	{}
}

<COMMENT>
{
	"/*"	{commentCount++;}
	"*/"	{commentCount--; if(commentCount==0){BEGIN INITIAL;}}
	.	{}
}

