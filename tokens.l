%{
#include <string>
#include "node.h"
#include "parser.hpp"
#define SAVE_TOKEN yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t) (yylval.token = t)
extern "C" int yywrap() { return 1; }
%}

%%


[ \t\n]                 ;


":="                    return TOKEN(T_ASSIGNMENT);
"="			            return TOKEN(T_CEQ);
"<>"                    return TOKEN(T_CNE);
"<"                     return TOKEN(T_CLT);
"<="                    return TOKEN(T_CLE);
">"                     return TOKEN(T_CGT);
">="                    return TOKEN(T_CGE);


"("                     return TOKEN(T_OPAREN);
")"                     return TOKEN(T_CPAREN);
"."                     return TOKEN(T_DOT);
","                     return TOKEN(T_COMMA);
";"                     return TOKEN(T_SEMICOLON);
":"                     return TOKEN(T_COLON);


"+"                     return TOKEN(T_PLUS);
"-"                     return TOKEN(T_MINUS);
"*"                     return TOKEN(T_MUL);
"/"                     return TOKEN(T_DIV);


"and"                   return TOKEN(T_AND);
"or"                    return TOKEN(T_OR);


"program"               return TOKEN(T_PROGRAM);
"begin"		            return TOKEN(T_BEGIN);
"end"			        return TOKEN(T_END);
"var"			        return TOKEN(T_VAR);
"function"		        return TOKEN(T_FUNCTION);


"integer"		        return TOKEN(T_INTEGER);
"real"			        return TOKEN(T_REAL);


"write"                 {yylval.string=new std::string("write"); return T_IDENTIFIER;}

[0-9_]                  {yylval.number=atoi(yytext); return T_DIGIT;}

"if"                    return TOKEN(T_IF);
"then"                  return TOKEN(T_THEN);
"else"                  return TOKEN(T_ELSE);
"while"                 return TOKEN(T_WHILE);
"do"                    return TOKEN(T_DO);
"for"                   return TOKEN(T_FOR);
"to"                    return TOKEN(T_TO);


[a-zA-Z_][a-zA-Z0-9_]* SAVE_TOKEN; return T_IDENTIFIER;
\"[a-zA-Z0-9_]*\" SAVE_TOKEN; return T_STRING;

%%
