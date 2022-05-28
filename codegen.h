#include <stack>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/Support/raw_ostream.h>
#include "llvm/IR/Module.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Value.h>

using namespace llvm;
using namespace std;

class BlockNode;

class Symbol
{
    llvm::Value *value;
    Type::TypeID type;

public:
    Symbol(Type::TypeID type, llvm::Value *value) : value(value), type(type){};
    Value *getValue() { return value; };
    Type::TypeID getType() { return type; };
};

class Array
{
    llvm::Value *value;
    llvm::Value *size;
    Type::TypeID type;

public:
    Array(Type::TypeID type, llvm::Value *size, llvm::Value *value) : value(value), size(size), type(type){};
    Value *getValue() { return value; };
    Value *getSize() { return size; };
    Type::TypeID getType() { return type; };
};

class CodeGenBlock
{
public:
    BasicBlock *block;
    std::map<std::string, Symbol *> locals; // Локальные переменные
    std::map<string, bool> isFunctionArgument;
    std::string currentFunctionName; // Для return-a
    std::map<std::string, Array *> localArr;
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
                cout << name << endl;
                return (*it)->locals[name];
            }
        }
        cout << "null" << endl;
        return nullptr;
    };

    Array *getArrSymbol(std::string name)
    {
        for (auto it = blocks.rbegin(); it != blocks.rend(); it++)
        {
            if ((*it)->localArr.find(name) != (*it)->localArr.end())
            {
                cout << name << endl;
                return (*it)->localArr[name];
            }
        }
        cout << "null" << endl;
        return nullptr;
    };

    void generateCode(BlockNode &root);
    GenericValue runCode();
    std::map<std::string, Symbol *> &locals() { return blocks.back()->locals; }
    std::map<std::string, Array *> &localArr() { return blocks.back()->localArr; }
    std::string &currentFunctionName() { return blocks.back()->currentFunctionName; };
    std::map<std::string, Symbol *> &getFunctions() { return functions; };
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

    bool isFunctionArgument(string name) const
    {

        for (auto it = blocks.rbegin(); it != blocks.rend(); it++)
        {
            if ((*it)->isFunctionArgument.find(name) != (*it)->isFunctionArgument.end())
            {
                return (*it)->isFunctionArgument[name];
            }
        }
        return false;
    }

        void setFuncArg(string name, bool value){
        cout << "Set " << name << " as func arg" << endl;
        blocks.back()->isFunctionArgument[name] = value;
    }
};

class CodeGenFunc {
public:

static llvm::Value *writeFunc(Module *module, IRBuilder<> *builder, LLVMContext* context, std::vector<llvm::Value *> args, bool newLine){
    
    FunctionCallee CalleeF = module->getOrInsertFunction("printf",
                                                        FunctionType::get(IntegerType::getInt32Ty(*context),
                                                        PointerType::get(Type::getInt8Ty(*context), 0), true /* this is var arg func type*/));
    std::string newformat;
    if (newLine == true) {
        for(auto it=args.begin(); it!=args.end(); it++){
            std::cout << (*it)->getType()->getTypeID() << std::endl;
            if ((*it)->getType()->getTypeID()==llvm::Type::PointerTyID) {
                newformat.append("%s\n");
            } else if((*it)->getType()->getTypeID()==llvm::Type::IntegerTyID) {
                newformat.append("%i\n");
            } else if ((*it)->getType()->getTypeID()==llvm::Type::DoubleTyID) {
                newformat.append("%f\n");
            }    
        }
    } else {
        for(auto it=args.begin(); it!=args.end(); it++){
            std::cout << (*it)->getType()->getTypeID() << std::endl;
            if ((*it)->getType()->getTypeID()==llvm::Type::PointerTyID) {
                newformat.append("%s");
            } else if((*it)->getType()->getTypeID()==llvm::Type::IntegerTyID) {
                newformat.append("%i");
            } else if ((*it)->getType()->getTypeID()==llvm::Type::DoubleTyID) {
                newformat.append("%f");
            }    
        }
    }
    llvm::Value *strVal = builder->CreateGlobalStringPtr(newformat);
    args.insert(args.begin(), strVal);
    builder->CreateCall(CalleeF, args, "printfCall");
    return nullptr; // для варнинга
    }
};

Value *LogErrorV(const char *str);
Value *LogErrorV(std::string str);