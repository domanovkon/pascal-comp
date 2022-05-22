#ifndef __NODE_H__
#define __NODE_H__

#include <iostream>
#include <vector>
#include <math.h> 
#include <llvm/IR/Value.h>

class CodeGenContext;
class NStatement;
class NExpression;
class NIdentifier;
class NVariableDeclaration;
class NVariableDefinition;
class NFunctionDeclaration;
class NDeclarations;

typedef int Number;
typedef std::vector<NStatement *> StatementList;
typedef std::vector<NExpression *> ExpressionList;
typedef std::vector<NIdentifier *> IdentifierList;
typedef std::vector<NVariableDeclaration *> VariableList;
typedef std::vector<std::vector<NVariableDeclaration *> *> LocalVariableList;
typedef std::vector<NDeclarations *> DeclarationsList;


class Node
{
public:
    virtual ~Node() {}
    virtual std::string getTypeName() const = 0;
    virtual llvm::Value *codeGen(CodeGenContext &context) {
        return nullptr;
    }
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
    virtual llvm::Value *codeGen(CodeGenContext &context);
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
    virtual llvm::Value *codeGen(CodeGenContext &context);

    void print()
    {
        std::cout << "Variable Declaration " << &id <<std::endl;
    }
};


class NDeclarations : public NStatement
{
public:
    std::vector<NIdentifier*> &identList;
    NIdentifier &type;

    NDeclarations(std::vector<NIdentifier*> &identList, NIdentifier &type) : identList(identList), type(type) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
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
    std::vector<NDeclarations*> declVarLists;
    NBlock() {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
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
    std::vector<NDeclarations*> *decllist;

    void print()
    {
        std::cout << "Function = '" << header.id.name << "' have "
                  << "Statements " << body.statements.size() << std::endl;
    }

    NFunctionDeclaration(NFunctionHeaderDeclaration &header, NBlock &body) : header(header), body(body)
    {
    }

    NFunctionDeclaration(NFunctionHeaderDeclaration &header, NBlock &body, std::vector<NDeclarations*> *decllist) : header(header), body(body), decllist(decllist)
    {
    }

    virtual llvm::Value *codeGen(CodeGenContext &context);
};


class NBinaryOperator : public NExpression
{
public:
    int op;
    NExpression &lhs;
    NExpression &rhs;
    NBinaryOperator(NExpression &lhs, int op, NExpression &rhs) : lhs(lhs), rhs(rhs), op(op)
    {
        std::cout << "Binary Operator";
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
    NReal(double value) : value(value) {
        std::cout << "Real value: " << value << "\n";
    }
    virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "NReal";
    }
};


class NConstantString : public NExpression
{
public:
    std::string value;
    NConstantString(std::string value) : value(value) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "NConstantString";
    }
};


class NLoopStatement : public NStatement
{
public:
    NIdentifier *initial;
    NExpression *condition;
    NInteger *fromA, *toB;
    NBlock *block;

    NLoopStatement() {}
    NLoopStatement(NBlock *b, NIdentifier *initIdent = nullptr, NExpression *cond = nullptr, NInteger *fromA = nullptr, NInteger *toB = nullptr): block(b), initial(initIdent), condition(cond), fromA(fromA), toB(toB) 
    {
        if (condition == nullptr)
        {
            condition = new NInteger(1);
        }
    }
    llvm::Value *codeGen(CodeGenContext &context) override;

    std::string getTypeName() const override
    {
        return "NLoopStatement";
    }
};


class NIfStatement : public NStatement
{
public:
    NExpression &condition;
    NBlock &trueBlock; // не может быть пустым
    NBlock *falseBlock; // может быть пустым

    NIfStatement(NExpression &cond, NBlock &blk, NBlock *blk2 = nullptr)
        : condition(cond), trueBlock(blk), falseBlock(blk2)
    {
    }

    llvm::Value *codeGen(CodeGenContext &context) override;

    std::string getTypeName() const override
    {
        return "NIfStatement";
    }
};


class NAssignment : public NStatement
{
public:
    NIdentifier &lhs;
    NExpression &rhs;
    NAssignment(NIdentifier &lhs, NExpression &rhs) : lhs(lhs), rhs(rhs)
    {
        std::cout << ":)";
    }
    virtual llvm::Value *codeGen(CodeGenContext &context);

    std::string getTypeName() const override
    {
        return "NAssignment";
    }
};


class NMethodCall : public NStatement, public NExpression
{
public:
    const NIdentifier &id;
    ExpressionList arguments;
    NMethodCall(const NIdentifier &id, ExpressionList &arguments) : id(id), arguments(arguments) {}
    NMethodCall(const NIdentifier &id) : id(id) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "NMethodCall";
    }
};


#endif