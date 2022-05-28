
#include <llvm/IR/Value.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/Support/raw_ostream.h>
#include "llvm/Bitcode/BitcodeWriter.h"
#include "node.h"
#include "codegen.h"
#include "parser.hpp"
#define ISTYPE(value, id) (value->getType()->getTypeID() == id)

static Type *typeOf(const IdentifierNode *type, LLVMContext *ctx)
{
    if (type->name.compare("integer") == 0)
    {
        return Type::getInt64Ty(*ctx);
    }
    else if (type->name.compare("real") == 0)
    {
        return Type::getDoubleTy(*ctx);
    }
    else if (type->name.compare("array") == 0)
    {
        return Type::getInt64Ty(*ctx);
    }
    return Type::getVoidTy(*ctx);
}

void CodeGenContext::generateCode(BlockNode &root)
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

    std::error_code error;
    llvm::raw_fd_ostream file("out.bc", error, llvm::sys::fs::F_None);
    passManager.add(createPrintModulePass(file));
    passManager.add(createPrintModulePass(outs()));

    passManager.run(*module);
    
    return;
}

llvm::Value *AssignmentNode::codeGen(CodeGenContext &context)
{
    cout << "Generating assignment of " << left.name << " = " << endl;
    
    // Проверка на return
    if (left.name.compare(context.currentFunctionName()) == 0)
    {
        cout << "Generating return statement" << endl;
        Value *returnValue = right.codeGen(context);
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

    Value *dst = context.getSymbol(left.name)->getValue();
    
    if (!dst)
    {
        return LogErrorV("Undeclared variable");
    }

    Value *exp = right.codeGen(context);

    if (exp != NULL)
    {
        if (indx != nullptr) {
            Value *indexValue = indx->codeGen(context);
            // ConstantInt* CI = dyn_cast<ConstantInt>(context.getArrSymbol(this->left.name)->getSize());
            // ConstantInt* CIIndex = dyn_cast<ConstantInt>(indexValue);
            // if (CI->getSExtValue() < CIIndex->getSExtValue()) {
                // std::cerr << "index outside array" << std::endl;
                // return nullptr;
            // } else {
                auto element_ptr = context.builder.CreateGEP(dst, (llvm::Type::getInt64Ty(context.llvmContext), indexValue));
                context.builder.CreateStore(exp, element_ptr);
            // }
        }


        // if (count != nullptr) {
        //     Value *indx = ConstantInt::get(Type::getInt64Ty(context.llvmContext), this->count->value, true);
        //     ConstantInt* CI = dyn_cast<ConstantInt>(context.getArrSymbol(this->left.name)->getSize());
        //     if (CI->getSExtValue() < this->count->value) {
        //         std::cerr << "index outside array" << std::endl;
        //         return nullptr;
        //     } else {
        //     auto element_ptr = context.builder.CreateGEP(dst, llvm::ConstantInt::get(llvm::Type::getInt64Ty(context.llvmContext), count->value));
        //     context.builder.CreateStore(exp, element_ptr);
        //     }

        // }
        else {
            context.builder.CreateStore(exp, dst);
        }
    }
    return dst;
}

llvm::Value *DeclarationsNode::codeGen(CodeGenContext &context)
{
    cout << "Generating declaration list: " << endl;
    for (auto it = this->identList.begin(); it != this->identList.end(); it++){
        cout << *it << endl;
    }
    return NULL;
}

llvm::Value *BinaryOpNode::codeGen(CodeGenContext &context)
{
    cout << "Generating binary operator" << endl;

    Value *L = this->left.codeGen(context);
    Value *R = this->right.codeGen(context);
    bool floatValue = false;

    // Приведение типов
    if ((L->getType()->getTypeID() == Type::DoubleTyID) || (R->getType()->getTypeID() == Type::DoubleTyID))
    {
        floatValue = true;
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
        cout << "null expression" << endl;
        return nullptr;
    }

    switch (this->op)
    {
    case T_PLUS:
        return floatValue ? context.builder.CreateFAdd(L, R, "addftmp") : context.builder.CreateAdd(L, R, "addtmp");
    case T_MINUS:
        return floatValue ? context.builder.CreateFSub(L, R, "subftmp") : context.builder.CreateSub(L, R, "subtmp");
    case T_MUL:
        return floatValue ? context.builder.CreateFMul(L, R, "mulftmp") : context.builder.CreateMul(L, R, "multmp");
    case T_DIV:
        return floatValue ? context.builder.CreateFDiv(L, R, "divftmp") : context.builder.CreateSDiv(L, R, "divtmp");
    case T_AND:
        return floatValue ? LogErrorV("Real type has no AND operation") : context.builder.CreateAnd(L, R, "andtmp");
    case T_OR:
        return floatValue ? LogErrorV("Real type has no OR operation") : context.builder.CreateOr(L, R, "ortmp");

    case T_CLT:
        return floatValue ? context.builder.CreateFCmpULT(L, R, "cmpftmp") : context.builder.CreateICmpULT(L, R, "cmptmp");
    case T_CLE:
        return floatValue ? context.builder.CreateFCmpOLE(L, R, "cmpftmp") : context.builder.CreateICmpSLE(L, R, "cmptmp");
    case T_CGE:
        return floatValue ? context.builder.CreateFCmpOGE(L, R, "cmpftmp") : context.builder.CreateICmpSGE(L, R, "cmptmp");
    case T_CGT:
        return floatValue ? context.builder.CreateFCmpOGT(L, R, "cmpftmp") : context.builder.CreateICmpSGT(L, R, "cmptmp");
    case T_CNE:
        return floatValue ? context.builder.CreateFCmpONE(L, R, "cmpftmp") : context.builder.CreateICmpNE(L, R, "cmptmp");
    case T_CEQ:
        return floatValue ? context.builder.CreateFCmpOEQ(L, R, "cmpftmp") : context.builder.CreateICmpEQ(L, R, "cmptmp");
    default:
        return LogErrorV("Unknown binary operator");
    }
    return NULL;
}

llvm::Value *BlockNode::codeGen(CodeGenContext &context)
{
    cout << "Generating block" << endl;
    Value *last = nullptr;
    for (auto it = this->statements.begin(); it != this->statements.end(); it++){
        last = (*it)->codeGen(context);
        cout << last << endl;
    }
    return last;
}

llvm::Value *IntegerNode::codeGen(CodeGenContext &context)
{
    cout << "Generating integer: " << this->value << endl;
    return ConstantInt::get(Type::getInt64Ty(context.llvmContext), this->value, true);
}

llvm::Value *RealNode::codeGen(CodeGenContext &context)
{
    cout << "Generating real: " << this->value << endl;
    return ConstantFP::get(Type::getDoubleTy(context.llvmContext), this->value);
}

Value *StringNode::codeGen(CodeGenContext &context)
{
    std::cout << "Generating string: " << value << std::endl;
    value = value.substr(1, value.size() - 2);
    Value *strVal = context.builder.CreateGlobalStringPtr(value);
    return strVal;
}

llvm::Value *IdentifierNode::codeGen(CodeGenContext &context)
{
    std::cout << "Generating identifier reference: " << name << std::endl;

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
                std::cout << "Generating method call: " << name << std::endl;
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


Value *FunctionNode::codeGen(CodeGenContext &context)
{
    vector<Type *> argTypes;
    VariableList::const_iterator it;

    for (it = header.arguments.begin(); it != header.arguments.end(); it++)
    {
        argTypes.push_back(typeOf(&((**it).type), &context.llvmContext));
    }

    FunctionType *ftype = FunctionType::get(typeOf(&(header.type), &context.llvmContext), argTypes, false);

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
    else if (header.type.name.compare("array") == 0)
    {
        t = Type::IntegerTyID;
    }

    context.getFunctions()[header.id.name.c_str()] = new Symbol(t, bblock);

    auto origin_arg = header.arguments.begin();

    std::cout << "func args" << std::endl;

    for (auto &ir_arg_it : function->args())
    {
        ir_arg_it.setName((*origin_arg)->id.name);

        Type::TypeID t = Type::VoidTyID;
        if ((*origin_arg)->type.name.compare("integer") == 0)
        {
            Value *argAlloc;
            argAlloc = (*origin_arg)->codeGen(context);
            context.builder.CreateStore(&ir_arg_it, argAlloc, false);
            t = Type::IntegerTyID;
            context.locals()[(*origin_arg)->id.name] = new Symbol(t, argAlloc);

            Type::TypeID funcTp = Type::LabelTyID;
        
            context.setFuncArg((*origin_arg)->id.name, true);
            origin_arg++;
        }
        else if ((*origin_arg)->type.name.compare("real") == 0)
        {
            Value *argAlloc;
            argAlloc = (*origin_arg)->codeGen(context);
            context.builder.CreateStore(&ir_arg_it, argAlloc, false);
            t = Type::DoubleTyID;
            context.locals()[(*origin_arg)->id.name] = new Symbol(t, argAlloc);

            Type::TypeID funcTp = Type::LabelTyID;
        
            context.setFuncArg((*origin_arg)->id.name, true);
            origin_arg++;
        }
        else if ((*origin_arg)->type.name.compare("array") == 0)
        {
            t = Type::IntegerTyID;
            Value *size = ConstantInt::get(Type::getInt64Ty(context.llvmContext), 10, true);
            AllocaInst* inst = context.builder.CreateAlloca(Type::getInt64Ty(context.llvmContext), size, (*origin_arg)->id.name);
            context.builder.CreateStore(&ir_arg_it, inst, false);
            context.locals()[(*origin_arg)->id.name] = new Symbol(t, inst);
            context.localArr()[(*origin_arg)->id.name] = new Array(Type::IntegerTyID, size, inst);
            context.setFuncArg((*origin_arg)->id.name, true);
            origin_arg++;
        }
    }

    if(this->decllist != nullptr){
        for (auto &ir_it1 : this->decllist[0])
        {
            for (auto &ir_it2 : ir_it1->identList)
            {
                if (ir_it1->count == nullptr) {
                    Type::TypeID t = Type::VoidTyID;
                    if (ir_it1->type.name.compare("integer") == 0)
                    {
                        t = Type::IntegerTyID;
                        AllocaInst* inst = context.builder.CreateAlloca(Type::getInt64Ty(context.llvmContext), nullptr, ir_it2->name.c_str());
                        context.locals()[ir_it2->name.c_str()] = new Symbol(t, inst);    
                    }
                    else if (ir_it1->type.name.compare("real") == 0)
                    {
                        t = Type::DoubleTyID;
                        AllocaInst* inst = context.builder.CreateAlloca(Type::getDoubleTy(context.llvmContext), nullptr, ir_it2->name.c_str());
                        context.locals()[ir_it2->name.c_str()] = new Symbol(t, inst);
                    }
                } else {
                    if (ir_it1->type.name.compare("integer") == 0)
                    {
                        if (ir_it1->count->value <= 0) {
                            LogErrorV("Error array size");
                            break;
                        }
                        // ArrayType* arrayType = ArrayType::get(Type::getInt64Ty(context.llvmContext), ir_it1->count->value);
                        // AllocaInst* variable = new AllocaInst(arrayType, ir_it2->name.c_str());
                        // Type::getInt64Ty(context.llvmContext)
                        Value *size = ConstantInt::get(Type::getInt64Ty(context.llvmContext), ir_it1->count->value, true);
                        AllocaInst* inst = context.builder.CreateAlloca(Type::getInt64Ty(context.llvmContext), size, ir_it2->name.c_str());
                        context.locals()[ir_it2->name.c_str()] = new Symbol(Type::IntegerTyID, inst);
                        context.localArr()[ir_it2->name.c_str()] = new Array(Type::IntegerTyID, size, inst);
                        

                        if (ir_it1->integerList != nullptr) {
                            if (ir_it1->integerList[0].size() !=  0 && ir_it1->integerList[0].size() == ir_it1->count->value) {
                                int indexValue = 0;
                                for (auto &number : ir_it1->integerList[0]) {
                                    Value *indx = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context.llvmContext), indexValue);
                                    Value *num_value = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context.llvmContext), number);
                                    cout << number << endl;
                                    Value *dst = context.getArrSymbol(ir_it2->name.c_str())->getValue();
                                    auto element_ptr = context.builder.CreateGEP(dst, (llvm::Type::getInt64Ty(context.llvmContext), indx));
                                    context.builder.CreateStore(num_value, element_ptr);
                                    indexValue++;
                                }
                            } else {
                                string str = ir_it2->name.c_str();
                                throw std::runtime_error("invalid array initialization: " + str);
                            }
                        }
                    }
                }
            }        
        }
    }

    context.currentFunctionName() = (header.id.name);

    body.codeGen(context);

    if (context.getFunctions()[header.id.name]->getType() == Type::TypeID::VoidTyID) {
        if (context.lastBlock == nullptr) {
            ReturnInst::Create(context.llvmContext, bblock);
        } else {
            ReturnInst::Create(context.llvmContext, context.lastBlock);
            context.lastBlock = nullptr;
        }
    }
    
    context.lastBlock = nullptr;

    context.popBlock();
    std::cout << "Creating function: " << header.id.name.c_str() << std::endl;
    return function;
}

llvm::Value *ArrayIndexNode::codeGen(CodeGenContext &context)
{
    cout << "Generating array index" << endl;
    Value * kindx = this->keyIndex->codeGen(context);
    
    if (!kindx)
        return nullptr;
    // ConstantInt* CIkindx = dyn_cast<ConstantInt>(kindx);
    
    // ConstantInt* CI = dyn_cast<ConstantInt>(context.getArrSymbol(this->id.name)->getSize());
    // if (CI->getSExtValue() < CIkindx->getSExtValue()) {
        // std::cerr << "index outside array" << std::endl;
        // return nullptr;
    // } else {
    Value *dst = context.getArrSymbol(this->id.name)->getValue();
    // Value *x0 = context.builder.CreateExtractValue(context.builder.CreateLoad(dst), this->index->value);
    Value *x0 = context.builder.CreateGEP(llvm::Type::getInt64Ty(context.llvmContext), dst,kindx);
    Value *res = context.builder.CreateLoad(x0);
    return res;
    // }
}

llvm::Value *MethodCallNode::codeGen(CodeGenContext &context)
{ 
    cout << "Generating method call of " << this->id.name << endl;
    if (id.name.compare("write") == 0 || id.name.compare("writeln") == 0)
    {
        bool newLine = false;
        if (id.name.compare("writeln") == 0) {
            newLine = true;
        }
        std::vector<Value *> argsv;
        for (auto it = arguments.begin(); it != arguments.end(); it++)
        {
            argsv.push_back((*it)->codeGen(context));
            // проверка на неудачную кодогенерацию
            if (!argsv.back())
            {
                return nullptr;
            }
        }
        return CodeGenFunc::writeFunc(context.module, &context.builder, &context.llvmContext, argsv, newLine);
    }

    Function *calleeF = context.module->getFunction(this->id.name);

    if (!calleeF)
    {
        LogErrorV("Function name not found");
    }
    if (calleeF->arg_size() != this->arguments.size())
    {
        LogErrorV("Function arguments size not match, calleeF=" + std::to_string(calleeF->size()) + ", this->arguments=" + std::to_string(this->arguments.size()));
    }
    std::vector<Value *> argsv;
    for (auto it = this->arguments.begin(); it != this->arguments.end(); it++)
    {
        argsv.push_back((*it)->codeGen(context));
        if (!argsv.back())
        {
            return nullptr;
        }
    }
    if (context.getFunctions()[id.name]->getType() == Type::VoidTyID)
    {   
        return context.builder.CreateCall(calleeF, argsv);
    }
    return context.builder.CreateCall(calleeF, argsv, "calltmp");
}

llvm::Value *VarDeclarationNode::codeGen(CodeGenContext &context)
{
    cout << "Generating var declaration " << this->type.name << " " << this->id.name << endl;
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
    return inst;
}

static Value *CastToBoolean(CodeGenContext &context, Value *condValue)
{

    if (ISTYPE(condValue, Type::IntegerTyID))
    {
        condValue = context.builder.CreateIntCast(condValue, Type::getInt1Ty(context.llvmContext), true);
        // Инструкция сравнения
        return context.builder.CreateICmpNE(condValue, ConstantInt::get(Type::getInt1Ty(context.llvmContext), 0, true));
    }
    else if (ISTYPE(condValue, Type::DoubleTyID))
    {
        return context.builder.CreateFCmpONE(condValue, ConstantFP::get(context.llvmContext, APFloat(0.0)));
    }
    else
    {
        return condValue;
    }
}

llvm::Value *LoopStatementNode::codeGen(CodeGenContext &context)
{
    cout << "Generating loop " << endl;

    // Получим текущую функци.
    Function *theFunction = context.builder.GetInsertBlock()->getParent();

    // Вставим новый блок в конце указанной функции
    BasicBlock *block = BasicBlock::Create(context.llvmContext, "forloop", theFunction);
    BasicBlock *after = BasicBlock::Create(context.llvmContext, "forcont");

    // Генерируем локальную переменную для for
    if (this->initial) {
        Value *dst = context.getSymbol(this->initial->name)->getValue();
        cout << "Generating Integer: " << this->fromNumber->value << endl;
        Value *exp = ConstantInt::get(Type::getInt64Ty(context.llvmContext), this->fromNumber->value, true);
        context.builder.CreateStore(exp, dst);
    }

    Value *condValue = nullptr;
    if (this->initial == nullptr) {
        // Генерируем условие для while
        condValue = this->condition->codeGen(context);
    } else {
        // Генерируем условие для for
        // Генерация инструкции сравнения, инструкция icmp »возвращает логическое значение или логический вектор на основе сравнения двух операндов целочисленного, целочисленного вектора, указателя или вектора указателя.
        // ult означает меньше чем
        // Эквивалент генерации такой инструкции% cmptmp = icmp ult i32% x,% addtmp
        condValue = context.builder.CreateICmpULT(
            new LoadInst(context.locals()[this->initial->name]->getValue(), "", false, context.builder.GetInsertBlock()), 
            ConstantInt::get(Type::getInt64Ty(context.llvmContext), this->toNumber->value, true), "cmptmp");
    }

    if (!condValue)
        return nullptr;

    condValue = CastToBoolean(context, condValue);

    // Создать инструкцию перехода
    context.builder.CreateCondBr(condValue, block, after);

    // Устанавливаем точку вставки, и все последующие вставки находятся в этом блоке
    context.builder.SetInsertPoint(block);

    context.pushBlock(block);

    this->block->codeGen(context);

    context.popBlock();

    if (this->initial) {
        // Инкремент
        Value *dst = context.getSymbol(this->initial->name)->getValue();
        Value *exp = context.builder.CreateAdd(
            new LoadInst(context.locals()[this->initial->name]->getValue(), "", false, context.builder.GetInsertBlock()),
            ConstantInt::get(Type::getInt64Ty(context.llvmContext), 1, true),"addftmp");
        context.builder.CreateStore(exp, dst);
    }

    if (this->initial == nullptr) {
        condValue = this->condition->codeGen(context);
    } else {
        condValue = context.builder.CreateICmpULT(
            new LoadInst(context.locals()[this->initial->name]->getValue(), "", false, context.builder.GetInsertBlock()), 
            ConstantInt::get(Type::getInt64Ty(context.llvmContext), this->toNumber->value, true), 
            "cmptmp"
            );
    }
    condValue = CastToBoolean(context, condValue);
    context.builder.CreateCondBr(condValue, block, after);

    // Вставляем следующий блок
    theFunction->getBasicBlockList().push_back(after);
    context.builder.SetInsertPoint(after);

    context.lastBlock = after;

    return nullptr;
}

llvm::Value *IfStatementNode::codeGen(CodeGenContext &context)
{
    cout << "Generating if" << endl;
    Value *condValue = this->condition.codeGen(context);
    if (!condValue)
        return nullptr;

    condValue = CastToBoolean(context, condValue);
    Function *theFunction = context.builder.GetInsertBlock()->getParent();
    /*
           Получив это, он создает три блока. Обратите внимание, что он передает «TheFunction» в конструктор блока «then». 
           Это заставляет конструктор автоматически вставлять новый блок в конце указанной функции. 
           Два других блока были созданы, но они не были вставлены в функцию.
     */
    // Создать блок кода для потом
    BasicBlock *thenBB = BasicBlock::Create(context.llvmContext, "then", theFunction);
    // Создать блок кода else
    BasicBlock *falseBB = BasicBlock::Create(context.llvmContext, "else");
    // продолжение
    BasicBlock *mergeBB = BasicBlock::Create(context.llvmContext, "ifcont");

    if (this->falseBlock)
    {
        // Создать условную инструкцию "br Cond, TrueDest, falseDest".
        //br i1 %ifcond, label %then, label %else
        context.builder.CreateCondBr(condValue, thenBB, falseBB);
    }
    else
    {
        context.builder.CreateCondBr(condValue, thenBB, mergeBB);
    }

     /**
           Вставив условную ветвь, мы переместим конструктор в блок «then». 
           Строго говоря, этот вызов перемещает точку вставки в конец указанного блока. 
           Однако, поскольку блок "then" пуст, он также начинается со вставки в начале блока, 
           который фактически должен установить точку вставки.
     */
    context.builder.SetInsertPoint(thenBB);
    context.pushBlock(thenBB);
    this->trueBlock.codeGen(context);
    context.popBlock();
    thenBB = context.builder.GetInsertBlock();
    if (thenBB->getTerminator() == nullptr)
    {
        // Чтобы завершить блок «then», мы создаем безусловную ветвь для блока слияния и 
        // создаем безусловную ветвь для блока слияния
        // это инструкция br label% ifcont
        context.builder.CreateBr(mergeBB);
    }
    // Если блок else существует, то повторяем для него действия блока then
    if (this->falseBlock)
    {
        theFunction->getBasicBlockList().push_back(falseBB);
        context.builder.SetInsertPoint(falseBB);
        context.pushBlock(thenBB);
        this->falseBlock->codeGen(context);
        context.popBlock();
        context.builder.CreateBr(mergeBB);
    }

    // Вставляем блок MergeBB:
    theFunction->getBasicBlockList().push_back(mergeBB);
    context.builder.SetInsertPoint(mergeBB);
    context.lastBlock = mergeBB;
    return nullptr;
}

std::unique_ptr<ExpressionNode> LogError(const char *str)
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
