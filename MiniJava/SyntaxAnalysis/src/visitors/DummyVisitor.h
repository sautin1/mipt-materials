// Description: CDummyVisitor - used for easy creation of other visitors
// just copy CDummyVisitor.cpp and CDummyVisitor.h and add all the necessary stuff (add methods, fields, complete `visit` methods)

#pragma once

#include <astNodes/AstNodeNames.h>
#include <Visitor.h>

#include <astNodes/AccessModifier.h>
#include <astNodes/Expression.h>
#include <astNodes/ExpressionList.h>
#include <astNodes/Statement.h>
#include <astNodes/StatementList.h>
#include <astNodes/TypeModifier.h>
#include <astNodes/VarDeclaration.h>
#include <astNodes/VarDeclarationList.h>
#include <astNodes/MethodArgument.h>
#include <astNodes/MethodArgumentList.h>
#include <astNodes/MethodDeclaration.h>
#include <astNodes/MethodDeclarationList.h>
#include <astNodes/MainClass.h>
#include <astNodes/ClassDeclaration.h>
#include <astNodes/ClassDeclarationList.h>
#include <astNodes/Program.h>

namespace AstTree {

class CDummyVisitor : public CVisitor {
public:
    CDummyVisitor( bool _verbose = false ) : CVisitor( _verbose ) {}
    ~CDummyVisitor() {}

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
};

}
