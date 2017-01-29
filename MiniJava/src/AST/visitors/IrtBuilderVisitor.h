#pragma once

#include <unordered_map>
#include <utility>

#include <AST/nodes/NodeNames.h>
#include <AST/visitors/Visitor.h>

#include <AST/nodes/AccessModifier.h>
#include <AST/nodes/Expression.h>
#include <AST/nodes/ExpressionList.h>
#include <AST/nodes/Statement.h>
#include <AST/nodes/StatementList.h>
#include <AST/nodes/TypeModifier.h>
#include <AST/nodes/VarDeclaration.h>
#include <AST/nodes/VarDeclarationList.h>
#include <AST/nodes/MethodArgument.h>
#include <AST/nodes/MethodArgumentList.h>
#include <AST/nodes/MethodDeclaration.h>
#include <AST/nodes/MethodDeclarationList.h>
#include <AST/nodes/MainClass.h>
#include <AST/nodes/ClassDeclaration.h>
#include <AST/nodes/ClassDeclarationList.h>
#include <AST/nodes/Program.h>

#include <IRT/nodes/Expression.h>
#include <IRT/nodes/ExpressionList.h>
#include <IRT/nodes/Statement.h>
#include <IRT/nodes/StatementList.h>

#include <IRT/SubtreeWrapper.h>

#include <IRT/Frame.h>

#include <SymbolTable.h>

namespace AstTree {

class CIrtBuilderVisitor : public CVisitor {
public:
    CIrtBuilderVisitor( std::shared_ptr<const CSymbolTable> table, bool _verbose = false )
        : CVisitor( _verbose ), symbolTable( table ) {}

    const std::unordered_map<std::string, std::unique_ptr<const IRTree::CStatement>>& MethodTrees() const;

    // Visitors for different node types.
    void Visit( const CPublicAccessModifier* modifier ) override;
    void Visit( const CPrivateAccessModifier* modifier ) override;

    void Visit( const CBinaryExpression* expression ) override;
    void Visit( const CBracketExpression* expression ) override;
    void Visit( const CNumberExpression* expression ) override;
    void Visit( const CLogicExpression* expression ) override;
    void Visit( const CIdExpression* expression ) override;
    void Visit( const CLengthExpression* expression ) override;
    void Visit( const CMethodExpression* expression ) override;
    void Visit( const CThisExpression* expression ) override;
    void Visit( const CNewArrayExpression* expression ) override;
    void Visit( const CNewIdExpression* expression ) override;
    void Visit( const CNegateExpression* expression ) override;

    void Visit( const CAssignIdStatement* statement ) override;
    void Visit( const CAssignIdWithIndexStatement* statement ) override;
    void Visit( const CPrintStatement* statement ) override;
    void Visit( const CConditionalStatement* statement ) override;
    void Visit( const CWhileLoopStatement* statement ) override;
    void Visit( const CBracesStatement* statement ) override;

    void Visit( const CIntTypeModifier* typeModifier ) override;
    void Visit( const CBooleanTypeModifier* typeModifier ) override;
    void Visit( const CIntArrayTypeModifier* typeModifier ) override;
    void Visit( const CIdTypeModifier* typeModifier ) override;

    void Visit( const CVarDeclaration* declaration ) override;
    void Visit( const CMethodArgument* argument ) override;
    void Visit( const CMethodDeclaration* declaration ) override;
    void Visit( const CMainClass* mainClass ) override;
    void Visit( const CClassDeclaration* declaration ) override;
    void Visit( const CProgram* program ) override;

    void Visit( const CExpressionList* list ) override;
    void Visit( const CStatementList* list ) override;
    void Visit( const CVarDeclarationList* list ) override;
    void Visit( const CMethodArgumentList* list ) override;
    void Visit( const CMethodDeclarationList* list ) override;
    void Visit( const CClassDeclarationList* list ) override;

private:
    IRTree::TOperatorType operatorFromAstToIr( TOperatorType type ) const;

    void updateSubtreeWrapper( const IRTree::ISubtreeWrapper* wrapperNew );

    std::string makeMethodFullName( const std::string& className, const std::string& methodName );

    std::unique_ptr<const IRTree::ISubtreeWrapper> subtreeWrapper;

    // frame
    std::shared_ptr<const CSymbolTable> symbolTable;

    std::string classCurrentName;
    std::unordered_map<std::string, std::unique_ptr<const IRTree::CFrame>> frames;
    std::unique_ptr<const IRTree::CFrame> frameCurrent;

    // result
    std::unordered_map<std::string, std::unique_ptr<const IRTree::CStatement>> methodTrees;
};

}
