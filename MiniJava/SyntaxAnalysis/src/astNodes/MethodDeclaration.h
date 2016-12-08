#pragma once

#include <memory>

#include <Visitor.h>
#include <VisitorTarget.h>

#include <AccessModifier.h>
#include <Expression.h>
#include <MethodArgumentList.h>
#include <StatementList.h>
#include <TypeModifier.h>
#include <VarDeclarationList.h>

namespace AstTree {

class CMethodDeclaration : public CVisitorTarget {
public:

    CMethodDeclaration( const CAccessModifier* _accessModifier, const CTypeModifier* _typeModifier,
            const CIdExpression* _methodId, const CMethodArgumentList* _methodArguments,
            const CVarDeclarationList* _varDeclarations, const CStatementList* _statements,
            const CExpression* _returnExpression, const CLocation& _location )
        : CVisitorTarget( _location ),
          accessModifier( _accessModifier ),
          typeModifier( _typeModifier ),
          methodId( _methodId ),
          methodArguments( _methodArguments ),
          varDeclarations( _varDeclarations ),
          statements( _statements ),
          returnExpression( _returnExpression ) {}

    const CAccessModifier* AccessModifier() const { return accessModifier.get(); }
    const CTypeModifier* TypeModifier() const { return typeModifier.get(); }
    const CIdExpression* MethodId() const { return methodId.get(); }
    const CMethodArgumentList* MethodArguments() const { return methodArguments.get(); }
    const CVarDeclarationList* VarDeclarations() const { return varDeclarations.get(); }
    const CStatementList* Statements() const { return statements.get(); }
    const CExpression* ReturnExpression() const { return returnExpression.get(); }

    void Accept( IVisitor* visitor ) const override { visitor->Visit( this ); }

private:
    std::unique_ptr<const CAccessModifier> accessModifier;
    std::unique_ptr<const CTypeModifier> typeModifier;
    std::unique_ptr<const CIdExpression> methodId;
    std::unique_ptr<const CMethodArgumentList> methodArguments;
    std::unique_ptr<const CVarDeclarationList> varDeclarations;
    std::unique_ptr<const CStatementList> statements;
    std::unique_ptr<const CExpression> returnExpression;
};

}
