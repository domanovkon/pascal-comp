#include <stack>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>
// #include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/IRBuilder.h>
// #include <llvm/ModuleProvider.h>
// #include <llvm/Target/TargetSelect.h>
#include <llvm/ExecutionEngine/GenericValue.h>
// #include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/raw_ostream.h>
#include "llvm/IR/Module.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Instructions.h>
// #include <llvm/Module.h>
// #include <llvm/Function.h>
// #include <llvm/Type.h>
// #include <llvm/DerivedTypes.h>
// #include <llvm/LLVMContext.h>
// #include <llvm/PassManager.h>
// #include <llvm/Instructions.h>
// #include <llvm/CallingConv.h>
// #include <llvm/Bitcode/ReaderWriter.h>
// #include <llvm/Analysis/Verifier.h>
// #include <llvm/Assembly/PrintModulePass.h>
// #include <llvm/Support/IRBuilder.h>
// #include <llvm/ModuleProvider.h>
// #include <llvm/Target/TargetSelect.h>
// #include <llvm/ExecutionEngine/GenericValue.h>
// #include <llvm/ExecutionEngine/JIT.h>
// #include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Value.h>

using namespace llvm;
using namespace std;

class NBlock;

class Symbol
{
    llvm::Value *value;
    Type::TypeID type;

public:
    Symbol(Type::TypeID type, llvm::Value *value) : value(value), type(type){};
    Value *getValue() { return value; };
    Type::TypeID getType() { return type; };
};

class CodeGenBlock
{
public:
    BasicBlock *block;
    std::map<std::string, Symbol *> locals;
    std::map<string, bool> isFuncArg;
};

class CodeGenContext
{
    std::vector<CodeGenBlock *> blocks;
    Function *mainFunction;
    std::map<std::string, Symbol *> functions;

public:
    BasicBlock *lastBlock = nullptr;
    LLVMContext llvmContext;
    Module *module;
    IRBuilder<> builder;

    CodeGenContext() : builder(llvmContext) { module = new Module("main", getGlobalContext()); }
    LLVMContext &getGlobalContext() { return llvmContext; }

    Symbol *getSymbol(std::string name)
    {
        for (auto it = blocks.rbegin(); it != blocks.rend(); it++)
        {
            if ((*it)->locals.find(name) != (*it)->locals.end())
            {
                // for (auto v : locals)
                    // std::cout << v-> << "\n";
                cout << name << endl;
                return (*it)->locals[name];
            }
        }
        cout << "null" << endl;
        return nullptr;
    };

    void generateCode(NBlock &root);
    GenericValue runCode();
    std::map<std::string, Symbol *> &locals() { return blocks.back()->locals; }
    std::map<std::string, Symbol *> &getFunctions() {
        return functions; //blocks.back()->functions; 
        };
    BasicBlock *currentBlock() { return blocks.back()->block; }

    std::vector<CodeGenBlock *> *getBlocks()
    {
        return &blocks;
    }
    void pushBlock(BasicBlock *block)
    {
        blocks.push_back(new CodeGenBlock());
        blocks.back()->block = block;
    }
    void popBlock()
    {
        CodeGenBlock *back = blocks.back();
        blocks.pop_back();
        delete back;
    }

    bool isFuncArg(string name) const
    {

        for (auto it = blocks.rbegin(); it != blocks.rend(); it++)
        {
            if ((*it)->isFuncArg.find(name) != (*it)->isFuncArg.end())
            {
                return (*it)->isFuncArg[name];
            }
        }
        return false;
    }

        void setFuncArg(string name, bool value){
        cout << "Set " << name << " as func arg" << endl;
        blocks.back()->isFuncArg[name] = value;
    }
};

Value *LogErrorV(const char *str);
Value *LogErrorV(std::string str);