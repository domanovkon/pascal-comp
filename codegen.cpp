
#include <llvm/IR/Value.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/Support/raw_ostream.h>
//#include <llvm/IR/Verifier.h>
#include "node.h"
#include "codegen.h"
#include "codegenProcedures.h"
#include "parser.hpp"
#define ISTYPE(value, id) (value->getType()->getTypeID() == id)

static Type *typeOf(const NIdentifier *type, LLVMContext *ctx)
{
    if (type->name.compare("integer") == 0)
    {
        return Type::getInt64Ty(*ctx);
    }
    else if (type->name.compare("real") == 0)
    {
        return Type::getDoubleTy(*ctx);
    }
    return Type::getVoidTy(*ctx);
}

static Value *CastToBoolean(CodeGenContext &context, Value *condValue)
{

    // if (ISTYPE(condValue, Type::IntegerTyID))
    // {
    //     condValue = context.builder.CreateIntCast(condValue, Type::getInt1Ty(context.llvmContext), true);
    //     return context.builder.CreateICmpNE(condValue, ConstantInt::get(Type::getInt1Ty(context.llvmContext), 0, true));
    // }
    // else if (ISTYPE(condValue, Type::DoubleTyID))
    // {
    //     return context.builder.CreateFCmpONE(condValue, ConstantFP::get(context.llvmContext, APFloat(0.0)));
    // }
    // else
    // {
    //     return condValue;
    // }
    return NULL;
}

void CodeGenContext::generateCode(NBlock &root)
{
    cout << "Generating IR code" << endl;

    std::vector<Type *> sysArgs;
    FunctionType *mainFuncType = FunctionType::get(Type::getVoidTy(this->llvmContext), makeArrayRef(sysArgs), false);
    Function *mainFunc = Function::Create(mainFuncType, GlobalValue::ExternalLinkage, "main");
    BasicBlock *block = BasicBlock::Create(this->llvmContext, "entry");

    pushBlock(block);
    Value *retValue = root.codeGen(*this);
    popBlock();

    cout << "Code generate success" << endl;

    legacy::PassManager passManager;
    passManager.add(createPrintModulePass(outs()));
    passManager.run(*module);
    return;
}

llvm::Value *NAssignment::codeGen(CodeGenContext &context)
{
    cout << "Generating assignment of " << lhs.name << " = " << endl;



    // cout << " func header "<< context.locals() << " = " << endl;
    // for (auto v : context.getFunctions())
        // std::cout << " func header "<<  << "\n";
    


    // Function *theFunction = context.builder.GetInsertBlock()->getParent();

    // std::cout << " func header "<< *theFunction  << "\n";

    if (lhs.name.compare("result") == 0)
    {
        cout << "Generating return statement" << endl;
        Value *returnValue = rhs.codeGen(context);
        Value *returnValue2 = nullptr;
        if (returnValue->getType()->getTypeID() == llvm::Type::IntegerTyID)
        {
            returnValue2 = context.builder.CreateRet(returnValue);
        }
        else if (returnValue->getType()->getTypeID() == llvm::Type::DoubleTyID)
        {
            returnValue2 = context.builder.CreateRet(returnValue);
        }
        return returnValue2;
    }

    cout << "1" << endl;

    Value *dst = context.getSymbol(lhs.name)->getValue();
    cout << "11" << endl;
    cout << lhs.name << endl;
    
    
    // auto dstType = context.getSymbolType(lhs->name);
    // string dstTypeStr = dstType->name;
    if (!dst)
    {
        cout << "2" << endl;
        return LogErrorV("Undeclared variable");
    }
    cout << "3" << endl;
    Value *exp = rhs.codeGen(context);
    cout << "4" << endl;

    // cout << "dst typeid = " << TypeSystem::llvmTypeToStr(context.typeSystem.getVarType(dstTypeStr)) << endl;
    // cout << "exp typeid = " << TypeSystem::llvmTypeToStr(exp) << endl;

    // exp = context.typeSystem.cast(exp, context.typeSystem.getVarType(dstTypeStr), context.currentBlock());
    if (exp != NULL)
    {
        context.builder.CreateStore(exp, dst);
    }
    return dst;
}

llvm::Value *NDeclarations::codeGen(CodeGenContext &context)
{
    cout << "Generating declaration list: " << endl;
    for (auto it = this->identList.begin(); it != this->identList.end(); it++){
        cout << *it << endl;
    }
}

llvm::Value *NBinaryOperator::codeGen(CodeGenContext &context)
{
    cout << "Generating binary operator" << endl;

    Value *L = this->lhs.codeGen(context);
    Value *R = this->rhs.codeGen(context);
    bool fp = false;


    // type cast
    if ((L->getType()->getTypeID() == Type::DoubleTyID) || (R->getType()->getTypeID() == Type::DoubleTyID))
    {
        fp = true;
        if ((R->getType()->getTypeID() != Type::DoubleTyID))
        {
            R = context.builder.CreateUIToFP(R, Type::getDoubleTy(context.llvmContext), "ftmp");
        }
        if ((L->getType()->getTypeID() != Type::DoubleTyID))
        {
            L = context.builder.CreateUIToFP(L, Type::getDoubleTy(context.llvmContext), "ftmp");
        }
    }

    if (!L || !R)
    {
        cout << "null Expr" << endl;
        return nullptr;
    }
    cout << "fp = " << (fp ? "true" : "false") << endl;

    switch (this->op)
    {
    case T_PLUS:
        return fp ? context.builder.CreateFAdd(L, R, "addftmp") : context.builder.CreateAdd(L, R, "addtmp");
    case T_MINUS:
        return fp ? context.builder.CreateFSub(L, R, "subftmp") : context.builder.CreateSub(L, R, "subtmp");
    case T_MUL:
        return fp ? context.builder.CreateFMul(L, R, "mulftmp") : context.builder.CreateMul(L, R, "multmp");
    case T_DIV:
        return fp ? context.builder.CreateFDiv(L, R, "divftmp") : context.builder.CreateSDiv(L, R, "divtmp");
    case T_AND:
        return fp ? LogErrorV("Real type has no AND operation") : context.builder.CreateAnd(L, R, "andtmp");
    case T_OR:
        return fp ? LogErrorV("Real type has no OR operation") : context.builder.CreateOr(L, R, "ortmp");

    case T_CLT:
        return fp ? context.builder.CreateFCmpULT(L, R, "cmpftmp") : context.builder.CreateICmpULT(L, R, "cmptmp");
    case T_CLE:
        return fp ? context.builder.CreateFCmpOLE(L, R, "cmpftmp") : context.builder.CreateICmpSLE(L, R, "cmptmp");
    case T_CGE:
        return fp ? context.builder.CreateFCmpOGE(L, R, "cmpftmp") : context.builder.CreateICmpSGE(L, R, "cmptmp");
    case T_CGT:
        return fp ? context.builder.CreateFCmpOGT(L, R, "cmpftmp") : context.builder.CreateICmpSGT(L, R, "cmptmp");
    case T_CNE:
        return fp ? context.builder.CreateFCmpONE(L, R, "cmpftmp") : context.builder.CreateICmpNE(L, R, "cmptmp");
    case T_CEQ:
        return fp ? context.builder.CreateFCmpOEQ(L, R, "cmpftmp") : context.builder.CreateICmpEQ(L, R, "cmptmp");
    default:
        return LogErrorV("Unknown binary operator");
    }
    return NULL;
}

llvm::Value *NBlock::codeGen(CodeGenContext &context)
{
    cout << "Generating block" << endl;
    Value *last = nullptr;
    for (auto it = this->statements.begin(); it != this->statements.end(); it++){
        last = (*it)->codeGen(context);
        cout << last << endl;
    }
    return last;
}

llvm::Value *NInteger::codeGen(CodeGenContext &context)
{
    cout << "Generating Integer: " << this->value << endl;
    return ConstantInt::get(Type::getInt64Ty(context.llvmContext), this->value, true);
}

llvm::Value *NReal::codeGen(CodeGenContext &context)
{
    cout << "Generating Double: " << this->value << endl;
    return ConstantFP::get(Type::getDoubleTy(context.llvmContext), this->value);
}

Value *NConstantString::codeGen(CodeGenContext &context)
{
    std::cout << "Creating constant string: " << value << std::endl;
    value = value.substr(1, value.size() - 2);
    Value *strVal = context.builder.CreateGlobalStringPtr(value);
    return strVal;
}

llvm::Value *NIdentifier::codeGen(CodeGenContext &context)
{
    std::cout << "Creating identifier reference: " << name << std::endl;

    for (auto it = context.getBlocks()->begin(); it != context.getBlocks()->end(); it++)
    {
        cout << (*it)->locals.size();

        if ((*it)->locals.find(name) == (*it)->locals.end())
        {
            std::cerr << "undeclared variable " << name << std::endl;

            Function *function = context.module->getFunction(name.c_str());
            if (function != NULL)
            {
                std::cerr << "but finded and called function " << name << std::endl;
                if (function->arg_size() == 0)
                {
                    LogErrorV("Function arguments size not match, calleeF=" + std::to_string(function->size()) + ", this->arguments=" + std::to_string(0));
                }
                std::vector<Value *> argsv;
                // context.builder.CreateCall(function, argsv, "calltmp");
                CallInst *call = CallInst::Create(function, argsv, "", context.currentBlock());
                std::cout << "Creating method call: " << name << std::endl;
                return call;
            }
        }
        else
        {
            return new LoadInst((*it)->locals[name]->getValue(), "", false, context.builder.GetInsertBlock());
        }
    }
    return NULL;
}

// llvm::Value *NExpressionStatement::codeGen(CodeGenContext &context)
// {
    // return this->expression.codeGen(context);
// }

Value *NFunctionDeclaration::codeGen(CodeGenContext &context) ///// ---------
{
    vector<Type *> argTypes;
    VariableList::const_iterator it;

    std::cout << "123: " << std::endl;
    
    for (it = header.arguments.begin(); it != header.arguments.end(); it++)
    {
        argTypes.push_back(typeOf(&((**it).type), &context.llvmContext));
    }

    // for (auto v : header.arguments)
        // std::cout << v->type.name << "\n";

    // std::cout <<  header.type.name << std::endl;
    

    std::cout << "1234: " << std::endl;

    
    FunctionType *ftype = FunctionType::get(typeOf(&(header.type), &context.llvmContext), argTypes, false);


    std::cout << "12345: " << std::endl;

    Function *function;
    if (header.id.name.compare("") == 0)
    {
        function = Function::Create(ftype, GlobalValue::LinkageTypes::ExternalLinkage, "main", context.module);
    }
    else
    {
        function = Function::Create(ftype, GlobalValue::InternalLinkage, header.id.name.c_str(), context.module);
    }

    BasicBlock *bblock = BasicBlock::Create(context.llvmContext, "entry", function, 0);
    context.builder.SetInsertPoint(bblock);

    context.pushBlock(bblock);

    Type::TypeID t = Type::VoidTyID;
    std::cout << "header type : " << header.type.name << std::endl;
    if (header.type.name.compare("integer") == 0)
    {
        t = Type::IntegerTyID;
        
    }
    else if (header.type.name.compare("real") == 0)
    {
        t = Type::DoubleTyID;
    }

    context.getFunctions()[header.id.name.c_str()] = new Symbol(t, bblock);

    auto origin_arg = header.arguments.begin();

    std::cout << "func args" << std::endl;

    for (auto &ir_arg_it : function->args())
    {

        // std::cout << ir_arg_it.getType() << std::endl;

        ir_arg_it.setName((*origin_arg)->id.name);
        Value *argAlloc;
        argAlloc = (*origin_arg)->codeGen(context);
        context.builder.CreateStore(&ir_arg_it, argAlloc, false);

        Type::TypeID t = Type::VoidTyID;
        if ((*origin_arg)->type.name.compare("integer") == 0)
        {
            t = Type::IntegerTyID;
        }
        else if ((*origin_arg)->type.name.compare("real") == 0)
        {
            t = Type::DoubleTyID;
        }

        context.locals()[(*origin_arg)->id.name] = new Symbol(t, argAlloc);

        Type::TypeID funcTp = Type::LabelTyID;
        // context.locals()[header.id.name + "fun"] = new Symbol(funcTp, nullptr);


        // context.locals()[id.name] = new Symbol(t, inst);
        
        
        context.setFuncArg((*origin_arg)->id.name, true);
        origin_arg++;
    }

    if(this->decllist != nullptr){
        for (auto &ir_it1 : this->decllist[0])
        {
            for (auto &ir_it2 : ir_it1->identList)
            {
                std::cout << ir_it2->name.c_str();
                AllocaInst* inst = context.builder.CreateAlloca(Type::getInt64Ty(context.llvmContext), nullptr, ir_it2->name.c_str());
                context.locals()[ir_it2->name.c_str()] = new Symbol(t, inst);

            }        
        }
    }

    body.codeGen(context);

    // if (header.id.name.compare("") == 0)
    // {
    //     ReturnInst::Create(context.context, bblock);
    // }

    if(context.getFunctions()[header.id.name]->getType() == Type::TypeID::VoidTyID){
        if(context.lastBlock==nullptr){
            ReturnInst::Create(context.llvmContext, bblock);
        } else{
            ReturnInst::Create(context.llvmContext, context.lastBlock);
            context.lastBlock = nullptr;
        }
    }
    
    context.lastBlock = nullptr;

    context.popBlock();
    std::cout << "Creating function: " << header.id.name.c_str() << std::endl;
    return function;
}


llvm::Value *NMethodCall::codeGen(CodeGenContext &context)
{ ////++++++++++++
//     cout << "Generating method call of " << this->id.name << endl;

//     if (id.name.compare("вывод") == 0)
//     {
//         std::string st("kek");
//         std::vector<Value *> argsv;
//         for (auto it = arguments.begin(); it != arguments.end(); it++)
//         {
//             argsv.push_back((*it)->codeGen(context));
//             if (!argsv.back())
//             { // if any argument codegen fail
//                 return nullptr;
//             }
//         }

//         return CodeGenProcedures::myprintf(context.module, &context.builder, &context.llvmContext, st, argsv);
//     }

//     Function *calleeF = context.module->getFunction(this->id.name);
//     if (!calleeF)
//     {
//         LogErrorV("Function name not found");
//     }
//     if (calleeF->arg_size() != this->arguments.size())
//     {
//         LogErrorV("Function arguments size not match, calleeF=" + std::to_string(calleeF->size()) + ", this->arguments=" + std::to_string(this->arguments.size()));
//     }
//     std::vector<Value *> argsv;
//     for (auto it = this->arguments.begin(); it != this->arguments.end(); it++)
//     {
//         argsv.push_back((*it)->codeGen(context));
//         if (!argsv.back())
//         { // if any argument codegen fail
//             return nullptr;
//         }
//     }
// if (context.getFunctions()[id.name]->getType() == Type::VoidTyID)
//     {
//         return context.builder.CreateCall(calleeF, argsv);
//     }
    // return context.builder.CreateCall(calleeF, argsv, "calltmp");
    return NULL;
}

llvm::Value *NVariableDeclaration::codeGen(CodeGenContext &context)
{
    cout << "Generating variable declaration of " << this->type.name << " " << this->id.name << endl;
    Type *type = typeOf(&this->type, &context.llvmContext);
    Value *initial = nullptr;

    Value *inst = nullptr;

    inst = context.builder.CreateAlloca(type);

    Type::TypeID t = Type::VoidTyID;
    if (this->type.name.compare("integer") == 0)
    {
        t = Type::IntegerTyID;
    }
    else if (this->type.name.compare("real") == 0)
    {
        t = Type::DoubleTyID;
    }
    context.locals()[id.name] = new Symbol(t, inst);

    if (this->assignmentExpr != nullptr)
    {
        NAssignment assignment(this->id, *this->assignmentExpr);
        assignment.codeGen(context);
    }
    return inst;
}

llvm::Value *NIfStatement::codeGen(CodeGenContext &context)
{
    // cout << "Generating if statement" << endl;
    // Value *condValue = this->condition.codeGen(context);
    // if (!condValue)
    //     return nullptr;

    // condValue = CastToBoolean(context, condValue);

    // Function *theFunction = context.builder.GetInsertBlock()->getParent(); // the function where if statement is in

    // BasicBlock *thenBB = BasicBlock::Create(context.llvmContext, "then", theFunction);
    // BasicBlock *falseBB = BasicBlock::Create(context.llvmContext, "else");
    // BasicBlock *mergeBB = BasicBlock::Create(context.llvmContext, "ifcont");

    // if (this->falseBlock)
    // {
    //     context.builder.CreateCondBr(condValue, thenBB, falseBB);
    // }
    // else
    // {
    //     context.builder.CreateCondBr(condValue, thenBB, mergeBB);
    // }

    // context.builder.SetInsertPoint(thenBB);

    // context.pushBlock(thenBB);

    // this->trueBlock.codeGen(context);

    // context.popBlock();

    // thenBB = context.builder.GetInsertBlock();

    // if (thenBB->getTerminator() == nullptr)
    // { //
    //     context.builder.CreateBr(mergeBB);
    // }

    // if (this->falseBlock)
    // {
    //     theFunction->getBasicBlockList().push_back(falseBB); //
    //     context.builder.SetInsertPoint(falseBB);             //

    //     context.pushBlock(thenBB);

    //     this->falseBlock->codeGen(context);

    //     context.popBlock();

    //     context.builder.CreateBr(mergeBB);
    // }

    // theFunction->getBasicBlockList().push_back(mergeBB); //
    // context.builder.SetInsertPoint(mergeBB);             //

    // context.lastBlock = mergeBB;

    return NULL;
    // return nullptr;
}

llvm::Value *NForStatement::codeGen(CodeGenContext &context)
{

//     Function *theFunction = context.builder.GetInsertBlock()->getParent();

//     BasicBlock *block = BasicBlock::Create(context.llvmContext, "forloop", theFunction);
//     BasicBlock *after = BasicBlock::Create(context.llvmContext, "forcont");

//   // execute the initial
//     if( this->initial ){
//         Value *dst = context.getSymbol(this->initial->name)->getValue();
//         cout << "Generating Integer: " << this->fromA->value << endl;
//         Value *exp = ConstantInt::get(Type::getInt64Ty(context.llvmContext), this->fromA->value, true);
//         context.builder.CreateStore(exp, dst);
//         // this->initial->codeGen(context);
//     }

//     Value *condValue = nullptr;
//     if(this->initial == nullptr){
//         condValue = this->condition->codeGen(context);
//     }else{
//         condValue = context.builder.CreateICmpULT(
//             new LoadInst(context.locals()[this->initial->name]->getValue(), "", false, context.builder.GetInsertBlock()), 
//             ConstantInt::get(Type::getInt64Ty(context.llvmContext), this->toB->value, true), 
//             "cmptmp"
//             );
//     }

//     if (!condValue)
//         return nullptr;

//     condValue = CastToBoolean(context, condValue);

//     // fall to the block
//     context.builder.CreateCondBr(condValue, block, after);

//     context.builder.SetInsertPoint(block);

//     context.pushBlock(block);

//     this->block->codeGen(context);

//     context.popBlock();


    // if( this->initial ){
    //     Value *dst = context.getSymbol(this->initial->name)->getValue();
    //     Value *exp = context.builder.CreateAdd(
    //         new LoadInst(context.locals()[this->initial->name]->getValue(), "", false, context.builder.GetInsertBlock()),
    //         ConstantInt::get(Type::getInt64Ty(context.llvmContext), 1, true),
    //         "addftmp");
    //     context.builder.CreateStore(exp, dst);
    // }

    // if(this->initial == nullptr){
    //     condValue = this->condition->codeGen(context);
    // }else{
    //     condValue = context.builder.CreateICmpULT(
    //         new LoadInst(context.locals()[this->initial->name]->getValue(), "", false, context.builder.GetInsertBlock()), 
    //         ConstantInt::get(Type::getInt64Ty(context.llvmContext), this->toB->value, true), 
    //         "cmptmp"
    //         );
    // }

    // condValue = CastToBoolean(context, condValue);
    // context.builder.CreateCondBr(condValue, block, after);

    // theFunction->getBasicBlockList().push_back(after);
    // context.builder.SetInsertPoint(after);

    // context.lastBlock = after;

    return NULL;
}

// /*
//  * Global Functions
//  *
//  */

std::unique_ptr<NExpression> LogError(const char *str)
{
    fprintf(stderr, "LogError: %s\n", str);
    return nullptr;
}

Value *LogErrorV(string str)
{
    return LogErrorV(str.c_str());
}

Value *LogErrorV(const char *str)
{
    LogError(str);
    return nullptr;
}
