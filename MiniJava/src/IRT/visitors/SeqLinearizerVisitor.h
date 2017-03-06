#pragma once

#include <deque>
#include <vector>

#include <IRT/nodes/NodeNames.h>
#include <IRT/visitors/Visitor.h>

#include <IRT/nodes/Expression.h>
#include <IRT/nodes/ExpressionList.h>
#include <IRT/nodes/Statement.h>

namespace IRTree {

class CSeqLinearizerVisitor : public CVisitor {
public:
    CSeqLinearizerVisitor( bool _verbose = false ) : CVisitor( _verbose ), distanceToSeqStack( 1, 0 ) {}
    ~CSeqLinearizerVisitor() {}

    // Visitors for different node types.
    void Visit( const CConstExpression* expression ) override;
    void Visit( const CNameExpression* expression ) override;
    void Visit( const CTempExpression* expression ) override;
    void Visit( const CBinaryExpression* expression ) override;
    void Visit( const CMemExpression* expression ) override;
    void Visit( const CCallExpression* expression ) override;
    void Visit( const CEseqExpression* expression ) override;

    void Visit( const CExpStatement* expression ) override;
    void Visit( const CJumpConditionalStatement* expression ) override;
    void Visit( const CJumpStatement* expression ) override;
    void Visit( const CLabelStatement* expression ) override;
    void Visit( const CMoveStatement* expression ) override;
    void Visit( const CSeqStatement* expression ) override;

    void Visit( const CExpressionList* list ) override;
    void Visit( const CStatementList* list ) override;
private:
    // std::vector<std::deque<std::unique_ptr<const CStatement>>> dequeStack; // stack of deques
    std::vector<bool> isAddToLeftStack;
    std::vector<int> distanceToSeqStack;
};

}
