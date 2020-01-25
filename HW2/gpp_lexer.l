/* CSE341 - Programming Languages, Assignment 2, G++ Lexer
 * Written by Fatih Koç, 141044013
 */

%{
/* includes */
%}

COMMENT 		;;.+\n
KEYWORD			"and"|"or"|"not"|"equal"|"less"|"nil"|"list"|"append"|"concat"|"set"|"deffun"|"for"|"if"|"exit"|"load"|"disp"|"true"|"false"
OPERATOR		"+"|"-"|"/"|"*"|"("|")"|"**"|"\""|","
VALUE			"0"|[1-9][0-9]*
IDENTIFIER		[a-zA-Z][a-zA-Z0-9]*

%%

[ \t\n]+		/* Ignore whitespaces */

{COMMENT}		{ printf( "COMMENT\n" ); }
{OPERATOR}		{ printf( "OP_%s\n", yytext ); }
{KEYWORD}		{ printf( "KW_%s\n", yytext ); }

{VALUE}			{ printf( "VALUE\n" ); }

{IDENTIFIER}	{ printf( "IDENTIFIER\n" ); }

.				{ printf( "SYNTAX_ERROR %s cannot be tokenized.\n", yytext ); return 1; }

%%

int yywrap(){}

int main (int argc, char **argv) {
	yyin = fopen( argv[1], "r" );
    yylex();
    return 0;
}