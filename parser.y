%{
    #include "node.h"
    #include <string>
    
    #include <iostream>
    NBlock *programBlock; /* корень */

    extern int yylex();
    extern "C" FILE *yyin;
    void yyerror(const char *s) { printf("ERROR: %s\n", s); }
    using namespace std;
%}

%union {
        NBlock* block;
        NStatement *stmt;
        std::string *string;
        NIdentifier *ident;
        // NVariableDeclaration *var_decl;
        std::vector<NIdentifier*> *identList;
        std::vector<NDeclarations*> * declVarLists;
        int number;
        int token;
        // NInteger *integ;
        NDeclarations *decl;



        NFunctionHeaderDeclaration *funchead;
        NExpression *expr;
        ExpressionList *expr_list;
        IdentifierList *ident_list;
}

/* Терминалы */
%token <string> T_IDENTIFIER
%token <token> T_OPAREN T_CPAREN T_COMMA T_SEMICOLON T_COLON
%token <token> T_BEGIN T_END
%token <token>  T_VAR
%token <token> T_INTEGER T_REAL
%token <token> T_FUNCTION  T_PROGRAM


/* Нетерминалы */
%type <block> program function statement-sequence procedure-body compound-statement
%type <ident> identifier type-identifier 
%type <stmt> statement procedure-declaration  
%type <funchead> procedure-heading
%type <declVarLists> declarations-list
%type <decl> declarations
%type <identList> identifier-list

%start program


%%

                
program : T_PROGRAM identifier T_SEMICOLON function  { programBlock = $4; std::cout << "program\n "; }
                | T_PROGRAM identifier T_SEMICOLON T_VAR declarations function  { programBlock = $6; programBlock ->statements.push_back($5); std::cout << "program\n "; }
                | function { programBlock = $1; std::cout << "program\n "; }
                ;             

                ;
function : procedure-declaration { $$ = new NBlock(); typeid($1).name(); $$->statements.push_back($1); std::cout << "function\n "; }
                |       function procedure-declaration { $1->statements.push_back($2); std::cout << "function\n ";}
                ;

procedure-declaration : procedure-heading procedure-body { $$ = new NFunctionDeclaration(*$1, *$2); std::cout << "procedure-declaration\n "; }           
                

procedure-heading : { $$ = new NFunctionHeaderDeclaration(); std::cout << "procedure-heading\n "; }
                /* |   TALG identifier { $$ = new NFunctionHeaderDeclaration(*$2); std::cout << "procedure-heading+identifier "<< $2->name << "\n "; } */
                /* |   TALG identifier TLPAREN formal-parameter-list TRPAREN  { $$ = new NFunctionHeaderDeclaration(*$2, *$4); std::cout << "procedure-heading+identifier+params\n "; } */
                /* |   TALG type-identifier identifier { $$ = new NFunctionHeaderDeclaration(*$3, *$2); std::cout << "procedure-heading+identifier \n "; } */
                |   T_FUNCTION identifier T_OPAREN  T_CPAREN type-identifier  { $$ = new NFunctionHeaderDeclaration(*$2, *$5); std::cout << "procedure-heading+identifier+params\n "; }
                ;

type-identifier : T_INTEGER {$$ = new NIdentifier("int"); std::cout << "type-identifier\n ";}
                | T_REAL {$$ = new NIdentifier("real"); std::cout << "type-identifier\n ";}
                ;


		
declarations-list : declarations T_SEMICOLON { $$ = new DeclarationsList(); $$->push_back($1); }
                | declarations-list declarations T_SEMICOLON { $$->push_back($2); }
		;

declarations : identifier-list T_COLON type-identifier {$$ = new NDeclarations($1, $3);}
		;

identifier-list : identifier { $$ = new IdentifierList(); $$->push_back($1); }
		| identifier-list T_COMMA identifier { $$->push_back($3); } 
		;

procedure-body : compound-statement { $$ = $1; std::cout << "procedure-body\n "; }
                ;
                
compound-statement : T_BEGIN T_END {$$ = new NBlock(); std::cout << "compound-statement\n "; }
                | T_BEGIN statement-sequence T_END {$$ = $2; std::cout << "compound-statement\n "; }
                ;
                

identifier : T_IDENTIFIER { $$ = new NIdentifier(*$1); std::cout << "identifier1 "<< $$->name <<"\n "; }
                ;

statement-sequence : statement {$$ = new NBlock(); $$->statements.push_back($1);}
                |    statement-sequence statement { $1->statements.push_back($2); }
                |    statement-sequence T_SEMICOLON { $$ = $1; }
                ;


statement : identifier {$$ = nullptr;}
                ;
    		
%%
