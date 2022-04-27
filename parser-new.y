%{
    #include <string>
    #include <node.h>
    
    #define YYDEBUG 1
    NBlock *programBlock; /* корень Дерева */

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
        NVariableDeclaration *var_decl;
        std::vector<NVariableDeclaration*> *varvec;
        int number;
        int token;
        NInteger *integ;



        NFunctionHeaderDeclaration *funchead;
        NExpression *expr;
        ExpressionList *expr_list;
}

/* Терминалы */
%token <string> T_IDENTIFIER
%token <number> T_DIGIT
%token <token> T_ASSIGNMENT;
%token <token> T_CEQ T_CNE T_CLT T_CLE T_CGT T_CGE
%token <token> T_PLUS T_MINUS T_MUL T_DIV
%token <token> T_AND T_OR T_NOT
%token <token> T_OPAREN T_CPAREN T_DOT T_COMMA T_SEMICOLON T_COLON
%token <token> T_IF T_THEN T_ELSE
%token <token> T_FOR T_WHILE T_TO T_DO
%token <token> T_BEGIN T_END
%token <token> T_INTEGER T_REAL T_VAR
%token <token> T_FUNCTION T_PROCEDURE T_PROGRAM


/* Нетерминалы */
%type <block> program declarations subprogram_declarations subprogram_declaration subprogram_head
%type <ident> identifier var-identifier type-identifier identifier-list type standart-type
%type <token> relational-operator addition-operator multiplication-operator sign
%type <expr> expression simple-expression factor term number
%type <number> unsigned-digit-sequence digit-sequence integer-number real-number
%type <stmt> compound_statement loop-statement statement optional_statements simple-statement assignment-statement define-var-list-statement procedure-statement while_stmt for_stmt if_stmt statement_list 
%type <args> arguments parameter_list

%start program

%left TPLUS TMINUS
%left TMUL TDIV
%left UMINUS

%%

//program :  { std::cout << "program\n "; }
//                ;
                
//block : declaration-part { $$ = $1; std::cout << "block\n "; }
//                ;

relational-operator : T_CEQ | T_CNE | T_CLT | T_CLE | T_CGT | T_CGE
                ;
                
addition-operator : T_PLUS | T_MINUS | T_OR 
                ;
                
multiplication-operator : T_MUL
    		| T_DIV
    		| T_AND
    		;
    		
sign : T_PLUS 
                | T_MINUS 
                ;
                
    
identifier : T_IDENTIFIER { $$ = new NIdentifier(*$1); std::cout << "identifier1 "<< $$->name <<"\n "; }
		;

identifier-list : identifier { $$ = $1; }
		| identifier-list T_COMMA identifier //{ $$ = $1 } 
		;
		
declarations : declarations T_VAR identifier-list T_COLON type T_SEMICOLON // {$$ = $1}
		|
		;
		

type : standard_type
		;

standard_type : T_INTEGER
		| T_REAL
		;
		
subprogram_declarations : subprogram_declarations subprogram_declaration T_SEMICOLON //{ $$ =$1 }
		|
		;

subprogram_declaration : subprogram_head declarations compound_statement //{ $$ = $1 };
		;	

subprogram_head : T_FUNCTION identifier arguments T_COLON standard_type T_SEMICOLON // {$$ = $1}
		| T_PROCEDURE identifier arguments T_SEMICOLON // {$$ = $1}
		;

arguments : T_OPAREN parameter_list T_CPAREN { $$ = $2; }
		| { $$ = NULL; }
		;

parameter_list : identifier-list T_COLON type //{ $$ = $1; }
		| parameter_list T_SEMICOLON identifier-list T_COLON type //{ $$ = $1; }
		;

//expression : simple-expression { $$ = $1; }
//                | simple-expression relational-operator simple-expression { $$ = new NBinaryOperator(*$1, $2, *$3); std::cout << "expression with relational operator \n";}
  //              ;    
  
  
expression : simple-expression { $$ = $1; }
                | simple-expression relational-operator simple-expression { $$ = new NBinaryOperator(*$1, $2, *$3); std::cout << "expression with relational operator \n";}
                ;
                
simple-expression : term { $$ = $1; }
                | simple-expression addition-operator term { $$ = new NBinaryOperator(*$1, $2, *$3); std::cout << "expression with addition operator \n";}
                ;
         
term : factor {$$ = $1; std::cout << "term \n";}
                | term multiplication-operator factor {$$ = new NBinaryOperator(*$1, $2, *$3); std::cout << "term with mul operator\n";}
                ;
                
                
                
factor : number { $$ = $1; std::cout << "numfactor \n";}
                | var-identifier { $$ = $1; std::cout << "varFactorIdentifer \n";}
                | T_OPAREN expression T_CPAREN { $$ = $2; std::cout << "exp factor \n";}
                | T_NOT factor { $$ = $1; std::cout << "not factor \n";}
                //| TSTRING { $$ = new NConstantString(*$1); std::cout << "strfactor \n";}
                ;
                
                
var-identifier : identifier {$$ =$1; std::cout << "var-identifier \n";}
                ;
                
number : integer-number { $$ = new NInteger($1);}
                | real-number { $$ = new NReal($1);}
                ;


integer-number : digit-sequence {$$ = $1;}
                ;
                
real-number : digit-sequence T_DOT 
                 | digit-sequence T_DOT digit-sequence
                 ;
                
digit-sequence : unsigned-digit-sequence {$$ = $1;}
                | sign unsigned-digit-sequence %prec UMINUS { if($1 == TMINUS) {$$=-1*$2; std::cout << "set Sign "<<$$<<"\n";} else { $$ = $2; } }
                ;
                
unsigned-digit-sequence : unsigned-digit-sequence T_DIGIT { $$ = $1 * 10 + $2; std::cout << "Current T_DIGIT "<<$2<<", all number "<<$$<<"\n";}
                | T_DIGIT { $$ = $1; std::cout << "Current TDIGIT "<<$1<<", all number "<<$$<<"\n";}
                ;

                
                
                
                
                
                
                
                
                
                
program : T_PROGRAM identifier T_OPAREN identifier-list T_CPAREN T_SEMICOLON
		declarations
		subprogram_declarations
		compound_statement
		T_DOT
		//{ $$ = $1; std::cout << "program\n ";}
		;         
                
                
                
                
compound_statement: T_BEGIN optional_statements T_END T_DOT { $$ = $2; }
    		| /* E */ { $$ = NULL; }
    		;
    	
optional_statements: statement_list { $$ = $1; }
    		| /* E */ { $$ = NULL; }
    		;
    		

statement_list : statement { $$ = $1; }
		| statement_list T_SEMICOLON statement //{ $$ = $1; }
		;
    		

statement : assignment-statement { $$ = $1; std::cout << "assignment-statement\n ";}
            	| loop-statement {$$ = $1; std::cout << "loop-statement\n ";}
            	| if_stmt {$$=$1; std::cout << "if_stmt\n ";}
                ;
                
                
assignment-statement : var-identifier T_ASSIGNMENT expression { $$ = new NAssignment(*$1, *$3); std::cout << "assignment-statement (, with ...)\n ";}
		;
		
	
if_stmt : T_IF expression T_THEN statement { $$ = new NIfStatement(*$2, *$4); }
		| T_IF expression T_THEN statement T_ELSE statement { $$ = new NIfStatement(*$2, *$4, $6); }
        ;
        
        
loop-statement : while_stmt {$$ = $1;}
                | for_stmt {$$ = $1;}
                ;

while_stmt : T_WHILE expression T_DO statement { $$ = new NForStatement($4, nullptr, $2); }
                ;

for_stmt : T_FOR var-identifier T_ASSIGNMENT number T_TO number T_DO statement { $$ = new NForStatement($8, $3, nullptr, $<integ>4, $<integ>6); }
    		

                

  
  
  
  
  
  
  
  
  
  

%%
