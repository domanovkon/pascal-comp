#ifndef __NODE_H__
#define __NODE_H__

#include <iostream>
#include <vector>
#include <llvm/IR/Value.h>

class CodeGenContext;
class NStatement;
class NExpression;
class NVariableDeclaration;
class NVariableDefinition;
class NFunctionDeclaration;

typedef int Number;
typedef std::vector<NStatement *> StatementList;
typedef std::vector<NExpression *> ExpressionList;
typedef std::vector<NVariableDeclaration *> VariableList;
typedef std::vector<std::vector<NVariableDeclaration *> *> LocalVariableList;



class NIdentifier : public NExpression
{
public:
    std::string name;
    NIdentifier() {}
    NIdentifier(std::string name) : name(name) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "NIdentifier";
    }
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
    virtual llvm::Value *codeGen(CodeGenContext &context);
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
    virtual llvm::Value *codeGen(CodeGenContext &context);
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
    virtual llvm::Value *codeGen(CodeGenContext &context);
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

    llvm::Value *codeGen(CodeGenContext &context) override;

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

    llvm::Value *codeGen(CodeGenContext &context) override;

    std::string getTypeName() const override
    {
        return "NIfStatement";
    }
};
