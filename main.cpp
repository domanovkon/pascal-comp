#include <iostream>
#include "codegen.h"
#include "node.h"
extern BlockNode* programBlock;
extern int yyparse();

int main(int argc, char **argv)
{
    
    yyparse();

    CodeGenContext context;
    context.generateCode(*programBlock);
    return 0;
}      
