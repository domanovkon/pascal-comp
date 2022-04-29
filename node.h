#ifndef __NODE_H__
#define __NODE_H__

#include <iostream>
#include <vector>
// #include <llvm/IR/Value.h>

class CodeGenContext;
class NStatement;
class NExpression;
class NIdentifier;
class NVariableDeclaration;
class NVariableDefinition;
class NFunctionDeclaration;

typedef int Number;
typedef std::vector<NStatement *> StatementList;
typedef std::vector<NExpression *> ExpressionList;
typedef std::vector<NIdentifier *> IdentifierList;
typedef std::vector<NVariableDeclaration *> VariableList;
typedef std::vector<std::vector<NVariableDeclaration *> *> LocalVariableList;


class Node
{
public:
    virtual ~Node() {}
    virtual std::string getTypeName() const = 0;
    // virtual llvm::Value *codeGen(CodeGenContext &context) {}
};



class NExpression : public Node
{
public:
    NExpression() {}

    std::string getTypeName() const override
    {
        return "NExpression";
    }
};

class NStatement : public Node
{
public:
    virtual void print()
    {
        std::cout << "" << std::endl;
    }
    std::string getTypeName() const override
    {
        return "NStatement";
    }
};


class NIdentifier : public NExpression
{
public:
    std::string name;
    NIdentifier() {}
    NIdentifier(std::string name) : name(name) {}
    // virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "NIdentifier";
    }
};


class NVariableDeclaration : public NStatement
{
public:
    const NIdentifier &type;
    NIdentifier &id;
    NExpression *assignmentExpr;
    NVariableDeclaration(const NIdentifier &type, NIdentifier &id) : type(type), id(id) {}
    NVariableDeclaration(const NIdentifier &type, NIdentifier &id, NExpression *assignmentExpr) : type(type), id(id), assignmentExpr(assignmentExpr) {}
    // virtual llvm::Value *codeGen(CodeGenContext &context);

    void print()
    {
        // std::cout << "" <<std::endl;
    }
};

class NDeclarations : public NStatement
{
public:
    // StatementList statements;
    // LocalVariableList localVars;
    std::vector<NIdentifier*> &identList;
    NIdentifier &type;

    NDeclarations(std::vector<NIdentifier*> &identList, NIdentifier &type) : identList(identList), type(type) {}
    // virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "NDeclarations";
    }
};

class NBlock : public NStatement
{
public:
    StatementList statements;
    LocalVariableList localVars;
    // NDeclarations localVars;
    NBlock() {}
    // virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "NBlock";
    }
};


class NFunctionHeaderDeclaration : public NStatement
{
public:
    NIdentifier type;
    NIdentifier id;
    VariableList arguments;

    NFunctionHeaderDeclaration() {}
    NFunctionHeaderDeclaration(NIdentifier id) : id(id)
    {
    }
    NFunctionHeaderDeclaration(NIdentifier id, VariableList arguments) : id(id), arguments(arguments)
    {
    }
    NFunctionHeaderDeclaration(NIdentifier id, NIdentifier type) : id(id), type(type)
    {
    }
    NFunctionHeaderDeclaration(NIdentifier id, NIdentifier type, VariableList arguments) : id(id), type(type), arguments(arguments)
    {
    }
};


class NFunctionDeclaration : public NStatement
{
public:
    NFunctionHeaderDeclaration &header;
    NBlock &body;

    void print()
    {
        std::cout << "Function with Id = '" << header.id.name << "' have "
                  << "statements " << body.statements.size() << std::endl;
    }

    // NBlock& block;

    // NFunctionDeclaration(NIdentifier* type, NIdentifier* id, VariableList* arguments, NBlock* block, VariableList* localVariabless){
    //     std::string idName = std::string();
    //     if(id!=0){
    //         idName = id->name;
    //     }
    //     std::cout << " Func {" << type << ", " << idName << ", " << arguments << ", " << block << " }\n";

    //     if(arguments!=NULL){
    //         for (int i = 0; i < (*arguments).size(); i++)
    //         {
    //             std::cout << arguments->at(i)->type.name << std::endl;
    //         }
    //     }
    // }

    NFunctionDeclaration(NFunctionHeaderDeclaration &header, NBlock &body) : header(header), body(body)
    {
    }
    // virtual llvm::Value *codeGen(CodeGenContext &context);
};


class NBinaryOperator : public NExpression
{
public:
    int op;
    NExpression &lhs;
    NExpression &rhs;
    NBinaryOperator(NExpression &lhs, int op, NExpression &rhs) : lhs(lhs), rhs(rhs), op(op)
    {
        std::cout << ":)";
    }
    // virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "NBinaryOperator";
    }
};



class NInteger : public NExpression
{
public:
    int value;
    NInteger(int value) : value(value) {}
    // virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "NInteger";
    }
};



class NReal : public NExpression
{
public:
    double value;
    NReal(double value) : value(value) {}
    // virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "NReal";
    }
};

class NForStatement : public NStatement
{
public:
    NIdentifier *initial;
    NExpression *condition;
    NInteger *fromA, *toB;
    NBlock *block;

    NForStatement() {}

    NForStatement(NBlock *b, NIdentifier *initIdent = nullptr, NExpression *cond = nullptr, NInteger *fromA = nullptr, NInteger *toB = nullptr)
        : block(b), initial(initIdent), condition(cond), fromA(fromA), toB(toB) 
    {
        if (condition == nullptr)
        {
            condition = new NInteger(1);
        }
    }

    // llvm::Value *codeGen(CodeGenContext &context) override;

    std::string getTypeName() const override
    {
        return "NForStatement";
    }
};

class NIfStatement : public NStatement
{
public:
    NExpression &condition;
    NBlock &trueBlock;  // should not be null
    NBlock *falseBlock; // can be null

    NIfStatement(NExpression &cond, NBlock &blk, NBlock *blk2 = nullptr)
        : condition(cond), trueBlock(blk), falseBlock(blk2)
    {
    }

    // string getTypeName() const override {
    //     return "NIfStatement";
    // }

    // llvm::Value *codeGen(CodeGenContext &context) override;

    std::string getTypeName() const override
    {
        return "NIfStatement";
    }
};
#endif