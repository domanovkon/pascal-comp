%{
    #include "node.h"
    #include <string>
    
    #include <iostream>
    BlockNode *programBlock;

    extern int yylex();
    extern "C" FILE *yyin;
    void yyerror(const char *s) { printf("ERROR: %s\n", s); }
    using namespace std;
%}

%union {
        BlockNode* block;
        StatementNode *stmt;
        std::string *string;
        IdentifierNode *ident;
        // ArrayIdentifierNode *arrayIdent;
        VarDeclarationNode *var_decl;
        std::vector<IdentifierNode*> *identList;
        std::vector<DeclarationsNode*> *declVarLists;
        std::vector<VarDeclarationNode*> *varvec;
        int number;
        double dblNumber;
        int token;
        IntegerNode *integ;
        DeclarationsNode *decl;



        FunctionHeaderNode *funchead;
        ExpressionNode *expr;
        ExpressionList *expr_list;
        IdentifierList *ident_list;
}

/* Терминалы */
%token <string> T_IDENTIFIER T_STRING
%token <token> T_OPAREN T_CPAREN T_COMMA T_SEMICOLON T_COLON T_DOT T_OSQPAREN T_CSQPAREN
%token <token> T_BEGIN T_END
%token <token>  T_VAR
%token <token> T_INTEGER T_REAL
%token <token> T_FUNCTION  T_PROGRAM
%token <token> T_ASSIGNMENT T_CEQ T_CNE T_CLT T_CLE T_CGT T_CGE
%token <token> T_PLUS T_MINUS T_MUL T_DIV
%token <token> T_AND T_OR
%token <token> T_IF T_THEN T_ELSE T_WHILE T_DO T_FOR T_TO T_ARRAY T_OF
%token <number> T_DIGIT;

/* Нетерминалы */
%type <block> program functions statement-sequence function-body compound-statement
%type <ident> identifier type-identifier var-identifier function-identifier
/* %type <arrayIdent> array-identifier */
%type <stmt> statement function-declaration assignment-statement repetitive-statement while_stmt for_stmt if_stmt simple-statement function-statement
%type <funchead> function-heading
%type <declVarLists> declarations-list
%type <decl> declarations
%type <identList> identifier-list
%type <expr> expression simple-expression term factor varfunc-designator number actual-parameter var-array-designator
%type <token> relational-operator addition-operator multiplication-operator sign
%type <expr_list> actual-parameter-list
%type <number> integer-number digit-sequence unsigned-digit-sequence
%type <dblNumber> real-number
%type <var_decl> formal-parameter-section
%type <varvec> formal-parameter-list

%start program
%left T_CLT T_CGT T_CNE T_CLE T_CGE T_CEQ
%left T_PLUS T_MINUS
%left T_MUL T_DIV
%left UMINUS

%%

                
program : T_PROGRAM identifier T_SEMICOLON functions T_DOT  { programBlock = $4; std::cout << "program\n "; }
                /* | T_PROGRAM identifier T_SEMICOLON T_VAR declarations-list function T_DOT { programBlock = $6; programBlock -> declVarLists = *$5 ; std::cout << "program\n "; } */
                | functions T_DOT { programBlock = $1; std::cout << "program\n "; }
                ;             

                
declarations-list : declarations T_SEMICOLON { $$ = new DeclarationsList(); $$->push_back($1); std::cout << "decl-list\n "; }
                | declarations-list declarations T_SEMICOLON { $$->push_back($2); std::cout << "decl-list\n ";}
		;


declarations : identifier-list T_COLON type-identifier {$$ = new DeclarationsNode(*$1, *$3); std::cout << "decl\n ";}
                | identifier-list T_COLON T_ARRAY T_OSQPAREN number T_CSQPAREN T_OF type-identifier {$$ = new DeclarationsNode(*$1, *$8, $<integ>5); std::cout << "array-decl\n ";}
		;                


identifier-list : identifier { $$ = new IdentifierList(); $$->push_back($1); std::cout << "ident-list\n ";}
		| identifier-list T_COMMA identifier { $$->push_back($3); std::cout << "ident-list\n ";} 
		;


functions : function-declaration { $$ = new BlockNode(); typeid($1).name(); $$->statements.push_back($1); std::cout << "functions\n "; }
                | functions function-declaration { $1->statements.push_back($2); std::cout << "functions\n ";}
                ;


function-declaration : function-heading function-body { $$ = new FunctionNode(*$1, *$2); std::cout << "function\n "; }           
                |  function-heading T_VAR declarations-list function-body { $$ = new FunctionNode(*$1, *$4, $3); std::cout << "function\n "; }
                ;


function-heading : { $$ = new FunctionHeaderNode(); std::cout << "function-heading\n "; }
                |   T_FUNCTION identifier T_OPAREN formal-parameter-list T_CPAREN T_COLON type-identifier { $$ = new FunctionHeaderNode(*$2, *$7, *$4); std::cout << "function-heading+identifier+args+type\n "; }
                |   T_FUNCTION identifier T_OPAREN T_CPAREN T_COLON type-identifier { $$ = new FunctionHeaderNode(*$2, *$6); std::cout << "function-heading+identifier+type\n "; }
                ;


formal-parameter-list : formal-parameter-list T_COMMA formal-parameter-section { $1->push_back($<var_decl>3); std::cout << "formal-parameter-list\n ";}
                | formal-parameter-section { $$ = new VariableList(); $$->push_back($<var_decl>1); std::cout << "formal-parameter-list\n ";}
                ;

  
formal-parameter-section : identifier T_COLON type-identifier  { $$ = new VarDeclarationNode(*$3, *$1);}
                ;


type-identifier : T_INTEGER {$$ = new IdentifierNode("integer"); std::cout << "integer-type-identifier\n ";}
                | T_REAL {$$ = new IdentifierNode("real"); std::cout << "real-type-identifier\n ";}
                | T_ARRAY {$$ = new IdentifierNode("array"); std::cout << "real-type-identifier\n ";}
                ;

/* array-identifier : T_IDENTIFIER T_OSQPAREN number T_CSQPAREN {$$ = new ArrayIdentifierNode(*$1, $<integ>3); std::cout << "array-identifier \n";} */
                /* ; */

var-identifier : identifier {$$ =$1; std::cout << "var-identifier \n";}
                ;


function-identifier : identifier {$$ =$1; std::cout << "function-identifier \n";}
                ;


function-body : compound-statement { $$ = $1; std::cout << "function-body\n "; }
                ;

                
compound-statement : T_BEGIN T_END {$$ = new BlockNode(); std::cout << "compound-statement\n "; }
                | T_BEGIN statement-sequence T_END {$$ = $2; std::cout << "compound-statement\n "; }
                ;


identifier : T_IDENTIFIER { $$ = new IdentifierNode(*$1); std::cout << "identifier "<< $$->name <<"\n "; }
                ;


statement-sequence : statement {$$ = new BlockNode(); $$->statements.push_back($1);}
                | statement-sequence statement { $1->statements.push_back($2); }
                | statement-sequence T_SEMICOLON { $$ = $1; }
                ;


statement : simple-statement { $$ = $1; std::cout << "simple-statement\n ";}
                | repetitive-statement {$$ = $1; std::cout << "repetitive-statement\n ";}
                | if_stmt {$$=$1; std::cout << "if_stmt\n ";}
                ;


simple-statement : assignment-statement { $$ = $1; std::cout << "assignment-statement\n ";}
                | function-statement { $$ = $1; std::cout << "function-statement\n ";}
                ;
                

function-statement : function-identifier T_OPAREN actual-parameter-list T_CPAREN { $$ = new MethodCallNode(*$1, *$3); std::cout << "function-call-statement with PARAMS\n";}
                ;


assignment-statement : var-identifier T_ASSIGNMENT expression { $$ = new AssignmentNode(*$1, *$3); std::cout << "assignment-statement var \n ";} |
                var-identifier T_OSQPAREN simple-expression T_CSQPAREN T_ASSIGNMENT expression { $$ = new AssignmentNode(*$1, *$6, $<integ>3); std::cout << "assignment-statement var \n ";}
                /* | array-identifier T_ASSIGNMENT expression { $$ = new AssignmentNode(*$1, *$3); std::cout << "assignment-statement var \n ";} */
                ;


repetitive-statement : while_stmt {$$ = $1;}
                | for_stmt {$$ = $1;}
                ;


while_stmt : T_WHILE expression T_DO T_BEGIN statement-sequence T_END T_SEMICOLON { $$ = new LoopStatementNode($5, nullptr, $2); }
                ;


for_stmt :  T_FOR identifier T_ASSIGNMENT number T_TO number T_DO T_BEGIN statement-sequence T_END T_SEMICOLON { $$ = new LoopStatementNode($9, $2, nullptr, $<integ>4, $<integ>6); }
                ;


if_stmt : T_IF expression T_THEN T_BEGIN statement-sequence T_END T_SEMICOLON{ $$ = new IfStatementNode(*$2, *$5); }
		| T_IF expression T_THEN T_BEGIN statement-sequence T_END T_ELSE T_BEGIN statement-sequence T_END T_SEMICOLON { $$ = new IfStatementNode(*$2, *$5, $9); }
                ;


expression : simple-expression { $$ = $1; }
                | simple-expression relational-operator simple-expression { $$ = new BinaryOpNode(*$1, $2, *$3); std::cout << "expression with relational operator \n";}
                ;


simple-expression : term { $$ = $1; }
                | simple-expression addition-operator term { $$ = new BinaryOpNode(*$1, $2, *$3); std::cout << "expression with addition operator \n";}
                ;


term : factor {$$ = $1; std::cout << "term \n";}
                | term multiplication-operator factor {$$ = new BinaryOpNode(*$1, $2, *$3); std::cout << "term with mul operator\n";}
                ;


factor : number { $$ = $1; std::cout << "numfactor \n";}
                | var-identifier { $$ = $1; std::cout << "varFactorIdentifer \n";}
                | varfunc-designator { $$ = $1; std::cout << "var-func-factor \n";}
                | T_OPAREN expression T_CPAREN { $$ = $2; std::cout << "expfactor \n";}
                | T_STRING { $$ = new StringNode(*$1); std::cout << "strfactor \n";}
                | var-array-designator { $$ = $1; std::cout << "var-array-factor \n";}
                ;


number : integer-number { $$ = new IntegerNode($1);}
                | real-number {$$ = new RealNode($1);}
                ;


integer-number : digit-sequence {$$ = $1; std::cout << "integer-number \n";}
                ;

        
real-number : digit-sequence T_DOT unsigned-digit-sequence { $$ = std::stod(to_string($1) + "." + to_string($3)); std::cout << "real-number \n";}
                ;


digit-sequence : unsigned-digit-sequence {$$ = $1;}
                | sign unsigned-digit-sequence %prec UMINUS { /* Для устранения конфликта выбора приоритета */ if($1 == T_MINUS) {$$=-1*$2; std::cout << "set Sign "<<$$<<"\n";} else { $$ = $2; } }
                ;


unsigned-digit-sequence : unsigned-digit-sequence T_DIGIT { $$ = $1 * 10 + $2; std::cout << "Current digit "<<$2<<", all number "<<$$<<"\n";}
                | T_DIGIT { $$ = $1; std::cout << "Current digit2 "<<$1<<", all number "<<$$<<"\n";}
                ;
                
var-array-designator : 
                /* identifier T_OSQPAREN number T_CSQPAREN { $$= new ArrayIndexNode (*$1, $<integ>3, nullptr); } */
                /* | */
                 identifier T_OSQPAREN simple-expression T_CSQPAREN { $$= new ArrayIndexNode (*$1, nullptr,$3); }
                ;

varfunc-designator : function-identifier T_OPAREN actual-parameter-list T_CPAREN { $$= new MethodCallNode(*$1, *$3); }
                ;


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
