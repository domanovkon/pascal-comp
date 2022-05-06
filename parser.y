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
        NVariableDeclaration *var_decl;
        std::vector<NIdentifier*> *identList;
        std::vector<NDeclarations*> *declVarLists;
        std::vector<NVariableDeclaration*> *varvec;
        int number;
        int token;
        NInteger *integ;
        NDeclarations *decl;



        NFunctionHeaderDeclaration *funchead;
        NExpression *expr;
        ExpressionList *expr_list;
        IdentifierList *ident_list;
}

/* Терминалы */
%token <string> T_IDENTIFIER T_STRING
%token <token> T_OPAREN T_CPAREN T_COMMA T_SEMICOLON T_COLON T_DOT
%token <token> T_BEGIN T_END
%token <token>  T_VAR
%token <token> T_INTEGER T_REAL
%token <token> T_FUNCTION  T_PROGRAM
%token <token> T_ASSIGNMENT T_CEQ T_CNE T_CLT T_CLE T_CGT T_CGE
%token <token> T_PLUS T_MINUS T_MUL T_DIV
%token <token> T_AND T_OR T_NOT
%token <token> T_IF T_THEN T_ELSE T_WHILE T_DO T_FOR T_TO
%token <number> T_DIGIT;


/* Нетерминалы */
%type <block> program function statement-sequence procedure-body compound-statement
%type <ident> identifier type-identifier var-identifier function-identifier procedure-identifier
%type <stmt> statement procedure-declaration assignment-statement loop-statement while_stmt for_stmt if_stmt simple-statement procedure-statement
%type <funchead> procedure-heading
%type <declVarLists> declarations-list
%type <decl> declarations
%type <identList> identifier-list
%type <expr> expression simple-expression term factor varfunc-designator number actual-parameter
%type <token> relational-operator addition-operator multiplication-operator sign
%type <expr_list> actual-parameter-list
%type <number> integer-number digit-sequence unsigned-digit-sequence real-number
%type <var_decl> formal-parameter-section
%type <varvec> formal-parameter-list

%start program
%left T_CLT T_CGT T_CNE T_CLE T_CGE
%left T_PLUS T_MINUS
%left T_MUL T_DIV
%left UMINUS

%%

                
program : T_PROGRAM identifier T_SEMICOLON function T_DOT  { programBlock = $4; std::cout << "program\n "; }
                | T_PROGRAM identifier T_SEMICOLON T_VAR declarations-list function T_DOT { programBlock = $6; programBlock -> declVarLists = *$5 ; std::cout << "program\n "; }
                | function T_DOT { programBlock = $1; std::cout << "program\n "; }
                ;             

                ;
function : procedure-declaration { $$ = new NBlock(); typeid($1).name(); $$->statements.push_back($1); std::cout << "function\n "; }
                |       function procedure-declaration { $1->statements.push_back($2); std::cout << "function\n ";}
                ;

procedure-declaration : procedure-heading procedure-body { $$ = new NFunctionDeclaration(*$1, *$2); std::cout << "procedure-declaration\n "; }           
                

procedure-heading : { $$ = new NFunctionHeaderDeclaration(); std::cout << "procedure-heading\n "; }
                |   T_FUNCTION identifier T_OPAREN formal-parameter-list T_CPAREN T_COLON type-identifier { $$ = new NFunctionHeaderDeclaration(*$2, *$7, *$4); std::cout << "procedure-heading+identifier+params\n "; }
                |   T_FUNCTION identifier T_OPAREN T_CPAREN T_COLON type-identifier { $$ = new NFunctionHeaderDeclaration(*$2, *$6); std::cout << "procedure-heading+identifier+params\n "; }
                ;

formal-parameter-list : formal-parameter-list T_COMMA formal-parameter-section { $1->push_back($<var_decl>3); std::cout << "formal-parameter-list\n ";}
                | formal-parameter-section { $$ = new VariableList(); $$->push_back($<var_decl>1); std::cout << "formal-parameter-list\n ";}
                ;
  
formal-parameter-section : identifier T_COLON type-identifier  { $$ = new NVariableDeclaration(*$3, *$1);}
                ;

type-identifier : T_INTEGER {$$ = new NIdentifier("int"); std::cout << "type-identifier\n ";}
                | T_REAL {$$ = new NIdentifier("real"); std::cout << "type-identifier\n ";}
                ;

var-identifier : identifier {$$ =$1; std::cout << "var-identifier \n";}
                ;

function-identifier : identifier {$$ =$1; std::cout << "function-identifier \n";}
                ;

		
declarations-list : declarations T_SEMICOLON { $$ = new DeclarationsList(); $$->push_back($1); std::cout << "decl-list\n "; }
                | declarations-list declarations T_SEMICOLON { $$->push_back($2); std::cout << "decl-list\n ";}
		;

declarations : identifier-list T_COLON type-identifier {$$ = new NDeclarations(*$1, *$3); std::cout << "decl\n ";}
		;

identifier-list : identifier { $$ = new IdentifierList(); $$->push_back($1); std::cout << "ident-list\n ";}
		| identifier-list T_COMMA identifier { $$->push_back($3); std::cout << "ident-list\n ";} 
		;

procedure-body : compound-statement { $$ = $1; std::cout << "procedure-body\n "; }
                ;
                
compound-statement : T_BEGIN T_END {$$ = new NBlock(); std::cout << "compound-statement\n "; }
                | T_BEGIN statement-sequence T_END {$$ = $2; std::cout << "compound-statement\n "; }
                ;
                

identifier : T_IDENTIFIER { $$ = new NIdentifier(*$1); std::cout << "identifier1 "<< $$->name <<"\n "; }
                ;

statement-sequence : statement {$$ = new NBlock(); $$->statements.push_back($1);}
                | statement-sequence statement { $1->statements.push_back($2); }
                | statement-sequence T_SEMICOLON { $$ = $1; }
                ;

statement : simple-statement { $$ = $1; std::cout << "simple-statement\n ";}
                | loop-statement {$$ = $1; std::cout << "loop-statement\n ";}
                | if_stmt {$$=$1; std::cout << "if_stmt\n ";}
                ;

simple-statement : assignment-statement { $$ = $1; std::cout << "assignment-statement\n ";}
                | procedure-statement { $$ = $1; std::cout << "procedure-statement\n ";}
                ;

procedure-statement : procedure-identifier T_OPAREN actual-parameter-list T_CPAREN { $$ = new NMethodCall(*$1, *$3); std::cout << "procedure-call-statement with PARAMS\n";}
                ;


procedure-identifier : identifier { $$ = $1; std::cout << "procedure-identifier \n";}
                ;

assignment-statement : var-identifier T_ASSIGNMENT expression { $$ = new NAssignment(*$1, *$3); std::cout << "assignment-statement (, with ...)\n ";}
                ;

loop-statement : while_stmt {$$ = $1;}
                | for_stmt {$$ = $1;}
                ;

while_stmt : T_WHILE expression T_DO T_BEGIN statement-sequence T_END { $$ = new NForStatement($5, nullptr, $2); }
                ;

for_stmt :  T_FOR identifier T_ASSIGNMENT number T_TO number T_DO T_BEGIN statement-sequence T_END { $$ = new NForStatement($9, $2, nullptr, $<integ>4, $<integ>6); }
                ;

if_stmt : T_IF expression T_THEN T_BEGIN statement-sequence T_END T_SEMICOLON{ $$ = new NIfStatement(*$2, *$5); }
		| T_IF expression T_THEN T_BEGIN statement-sequence T_END T_ELSE T_BEGIN statement-sequence T_END T_SEMICOLON { $$ = new NIfStatement(*$2, *$5, $9); }
                ;


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
                | varfunc-designator { $$ = $1; std::cout << "varfactor \n";}
                | T_OPAREN expression T_CPAREN { $$ = $2; std::cout << "expfactor \n";}
                // | TNOT factor {}
                | T_STRING { $$ = new NConstantString(*$1); std::cout << "strfactor \n";}
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
                | sign unsigned-digit-sequence %prec UMINUS { if($1 == T_MINUS) {$$=-1*$2; std::cout << "set Sign "<<$$<<"\n";} else { $$ = $2; } }
                ;

unsigned-digit-sequence : unsigned-digit-sequence T_DIGIT { $$ = $1 * 10 + $2; std::cout << "Current digit "<<$2<<", all number "<<$$<<"\n";}
                | T_DIGIT { $$ = $1; std::cout << "Current digit "<<$1<<", all number "<<$$<<"\n";}
                ;
                
 
varfunc-designator : function-identifier T_OPAREN actual-parameter-list T_CPAREN { $$= new NMethodCall(*$1, *$3); }

actual-parameter-list : actual-parameter { $$ = new ExpressionList(); $$->push_back($1); }
                | actual-parameter-list T_COMMA actual-parameter { $$->push_back($3); }
                ;

actual-parameter : expression {$$ = $1;}
                ;

relational-operator : T_CEQ { $$ = $1; }
                | T_CNE { $$ = $1; }
                | T_CLT { $$ = $1; }
                | T_CLE { $$ = $1; }
                | T_CGT { $$ = $1; }
                | T_CGE { $$ = $1; }
                ;

addition-operator : T_PLUS { $$ = $1; }
                | T_MINUS { $$ = $1; }
                | T_OR { $$ = $1; }
                ;

multiplication-operator : T_MUL { $$ = $1; }
                | T_DIV { $$ = $1; }
                | T_AND { $$ = $1; }
                ;

sign : T_PLUS { $$ = $1 ;}
                | T_MINUS { $$ = $1 ;}
                ;

%%
