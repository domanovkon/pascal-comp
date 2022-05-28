#ifndef __NODE_H__
#define __NODE_H__

#include <iostream>
#include <vector>
#include <math.h> 
#include <llvm/IR/Value.h>

class CodeGenContext;
class StatementNode;
class ExpressionNode;
class IdentifierNode;
// class ArrayIdentifierNode;
class VarDeclarationNode;
class NVariableDefinition;
class FunctionNode;
class DeclarationsNode;
class IntegerNode;

typedef int Number;
typedef std::vector<StatementNode *> StatementList;
typedef std::vector<ExpressionNode *> ExpressionList;
typedef std::vector<IdentifierNode *> IdentifierList;
typedef std::vector<VarDeclarationNode *> VariableList;
typedef std::vector<std::vector<VarDeclarationNode *> *> LocalVariableList;
typedef std::vector<DeclarationsNode *> DeclarationsList;
typedef std::vector<int> IntegerList;


class Node
{
public:
    virtual ~Node() {}
    virtual std::string getTypeName() const = 0;
    virtual llvm::Value *codeGen(CodeGenContext &context) {
        return nullptr;
    }
};


class ExpressionNode : public Node
{
public:
    ExpressionNode() {}

    std::string getTypeName() const override
    {
        return "ExpressionNode";
    }
};


class StatementNode : public Node
{
public:
    virtual void print()
    {
        std::cout << "" << std::endl;
    }
    std::string getTypeName() const override
    {
        return "StatementNode";
    }
};

class IntegerNode : public ExpressionNode
{
public:
    int value;
    IntegerNode(int value) : value(value) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "IntegerNode";
    }
};


class IdentifierNode : public ExpressionNode
{
public:
    std::string name;
    IdentifierNode() {}
    IdentifierNode(std::string name) : name(name) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "IdentifierNode";
    }
};

// class ArrayIdentifierNode : public ExpressionNode
// {
// public:
//     std::string name;
//     IntegerNode *count;

//     ArrayIdentifierNode() {}
//     ArrayIdentifierNode(std::string name, IntegerNode *count) : name(name), count(count) {}
//     virtual llvm::Value *codeGen(CodeGenContext &context);
//     std::string getTypeName() const override
//     {
//         return "ArrayIdentifierNode";
//     }
// };


class VarDeclarationNode : public StatementNode
{
public:
    const IdentifierNode &type;
    IdentifierNode &id;
    ExpressionNode *assignmentExpression;
    VarDeclarationNode(const IdentifierNode &type, IdentifierNode &id) : type(type), id(id) {}
    VarDeclarationNode(const IdentifierNode &type, IdentifierNode &id, ExpressionNode *assignmentExpression) : type(type), id(id), assignmentExpression(assignmentExpression) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);

    void print()
    {
        std::cout << "Variable Declaration " << &id <<std::endl;
    }
};

class DeclarationsNode : public StatementNode
{
public:
    std::vector<IdentifierNode*> &identList;
    IdentifierNode &type;
    IntegerNode *count;
    std::vector<int> *integerList;

    // DeclarationsNode(std::vector<IdentifierNode*> &identList, IdentifierNode &type) : identList(identList), type(type) {}
    DeclarationsNode(std::vector<IdentifierNode*> &identList, IdentifierNode &type, IntegerNode *count = nullptr, std::vector<int> *integerList = nullptr) : identList(identList), type(type), count(count), integerList(integerList) {
    }
    virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "DeclarationsNode";
    }
};


class BlockNode : public StatementNode
{
public:
    StatementList statements;
    LocalVariableList localVars;
    std::vector<DeclarationsNode*> declVarLists;
    BlockNode() {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "BlockNode";
    }
};


class FunctionHeaderNode : public StatementNode
{
public:
    IdentifierNode type;
    IdentifierNode id;
    VariableList arguments;

    FunctionHeaderNode() {}
    FunctionHeaderNode(IdentifierNode id, IdentifierNode type) : id(id), type(type){}
    FunctionHeaderNode(IdentifierNode id, IdentifierNode type, VariableList arguments) : id(id), type(type), arguments(arguments){}
};


class FunctionNode : public StatementNode
{
public:
    FunctionHeaderNode &header;
    BlockNode &body;
    std::vector<DeclarationsNode*> *decllist;

    void print()
    {
        std::cout << "Function = '" << header.id.name << "' have "
                  << "Statements " << body.statements.size() << std::endl;
    }

    FunctionNode(FunctionHeaderNode &header, BlockNode &body) : header(header), body(body)
    {
    }

    FunctionNode(FunctionHeaderNode &header, BlockNode &body, std::vector<DeclarationsNode*> *decllist) : header(header), body(body), decllist(decllist)
    {
    }

    virtual llvm::Value *codeGen(CodeGenContext &context);
};


class BinaryOpNode : public ExpressionNode
{
public:
    int op;
    ExpressionNode &left;
    ExpressionNode &right;
    BinaryOpNode(ExpressionNode &left, int op, ExpressionNode &right) : left(left), right(right), op(op)
    {
        std::cout << "Binary Op Node";
    }
    virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "BinaryOpNode";
    }
};


class RealNode : public ExpressionNode
{
public:
    double value;
    RealNode(double value) : value(value) {
        std::cout << "Real value: " << value << "\n";
    }
    virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "RealNode";
    }
};


class StringNode : public ExpressionNode
{
public:
    std::string value;
    StringNode(std::string value) : value(value) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "StringNode";
    }
};


class LoopStatementNode : public StatementNode
{
public:
    IdentifierNode *initial;
    ExpressionNode *condition;
    IntegerNode *fromNumber, *toNumber;
    BlockNode *block;

    LoopStatementNode() {}
    LoopStatementNode(BlockNode *b, IdentifierNode *initIdent = nullptr, ExpressionNode *cond = nullptr, IntegerNode *fromNumber = nullptr, IntegerNode *toNumber = nullptr): block(b), initial(initIdent), condition(cond), fromNumber(fromNumber), toNumber(toNumber) 
    {
        if (condition == nullptr)
        {
            condition = new IntegerNode(1);
        }
    }
    llvm::Value *codeGen(CodeGenContext &context) override;

    std::string getTypeName() const override
    {
        return "LoopStatementNode";
    }
};


class IfStatementNode : public StatementNode
{
public:
    ExpressionNode &condition;
    BlockNode &trueBlock; // не может быть пустым
    BlockNode *falseBlock; // может быть пустым

    IfStatementNode(ExpressionNode &cond, BlockNode &blk, BlockNode *blk2 = nullptr)
        : condition(cond), trueBlock(blk), falseBlock(blk2)
    {}

    llvm::Value *codeGen(CodeGenContext &context) override;

    std::string getTypeName() const override
    {
        return "IfStatementNode";
    }
};


class AssignmentNode : public StatementNode
{
public:
    IdentifierNode &left;
    ExpressionNode &right;
    ExpressionNode *indx;
    // IntegerNode *count;
    AssignmentNode(IdentifierNode &left, ExpressionNode &right, ExpressionNode *indx = nullptr) : left(left), right(right), indx(indx) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);

    std::string getTypeName() const override
    {
        return "AssignmentNode";
    }
};


class MethodCallNode : public StatementNode, public ExpressionNode
{
public:
    const IdentifierNode &id;
    ExpressionList arguments;
    MethodCallNode(const IdentifierNode &id, ExpressionList &arguments) : id(id), arguments(arguments) {}
    MethodCallNode(const IdentifierNode &id) : id(id) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "MethodCallNode";
    }
};

class ArrayIndexNode : public StatementNode, public ExpressionNode
{
public:
    const IdentifierNode &id;
    IntegerNode *index;
    ExpressionNode *keyIndex;
    ArrayIndexNode(const IdentifierNode &id, IntegerNode *index = nullptr, ExpressionNode *keyIndex = nullptr) : id(id), index(index), keyIndex(keyIndex) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
    std::string getTypeName() const override
    {
        return "ArrayIndexNode";
    }
};


#endif