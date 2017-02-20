#pragma once

#include <memory>
#include <IRT/Label.h>
#include <IRT/nodes/VisitorTarget.h>

namespace IRTree {

class IStatement : public IVisitorTarget {
public:
    virtual ~IStatement();
};

class CStatement : public IStatement {
public:
    CStatement();
    virtual ~CStatement();
};

enum class TLogicOperatorType : char {
    LOT_EQ,
    LOT_NE,
    LOT_LT,
    LOT_GT,
    LOT_LE,
    LOT_GE,
    LOT_ULT,
    LOT_ULE,
    LOT_UGT,
    LOT_UGE
};

//-----------------------------------------------------------------------------------------------//
class CExpression;

class CMoveStatement : public CStatement {
public:
    CMoveStatement( const CExpression* _destination, const CExpression* _source );
    CMoveStatement( std::unique_ptr<const CExpression> _destination, std::unique_ptr<const CExpression> _source );
    ~CMoveStatement();

    const CExpression* Destination() const { return destination.get(); }
    const CExpression* Source() const { return source.get(); }

    void Accept( IVisitor* visitor ) const override { visitor->Visit( this ); }

private:
    std::unique_ptr<const CExpression> destination;
    std::unique_ptr<const CExpression> source;
};

//-----------------------------------------------------------------------------------------------//

class CExpStatement : public CStatement {
public:
    CExpStatement( const CExpression* _expression );
    CExpStatement( std::unique_ptr<const CExpression> _expression );
    ~CExpStatement();

    const CExpression* Expression() const { return expression.get(); }

    void Accept( IVisitor* visitor ) const override { visitor->Visit( this ); }

private:
    std::unique_ptr<const CExpression> expression;
};

//-----------------------------------------------------------------------------------------------//

class CJumpStatement : public CStatement {
public:
    CJumpStatement( CLabel _target );
    ~CJumpStatement();

    CLabel Target() const { return target; }

    void Accept( IVisitor* visitor ) const override { visitor->Visit( this ); }

private:
    CLabel target;
};

//-----------------------------------------------------------------------------------------------//

class CJumpConditionalStatement : public CStatement {
public:
    CJumpConditionalStatement( TLogicOperatorType _operation,
        const CExpression* left, const CExpression* right,
        CLabel _labelTrue, CLabel _labelFalse );
    CJumpConditionalStatement( TLogicOperatorType _operation,
    std::unique_ptr<const CExpression> left, std::unique_ptr<const CExpression> right,
    CLabel _labelTrue, CLabel _labelFalse );
    ~CJumpConditionalStatement();

    const CExpression* LeftOperand() const { return leftOperand.get(); }
    const CExpression* RightOperand() const { return rightOperand.get(); }
    CLabel TrueLabel() const { return labelTrue; }
    CLabel FalseLabel() const { return labelFalse; }
    TLogicOperatorType Operation() const { return operation; }

    void Accept( IVisitor* visitor ) const override { visitor->Visit( this ); }

private:
    std::unique_ptr<const CExpression> leftOperand;
    std::unique_ptr<const CExpression> rightOperand;
    CLabel labelTrue;
    CLabel labelFalse;
    TLogicOperatorType operation;
};

//-----------------------------------------------------------------------------------------------//

class CSeqStatement : public CStatement {
public:
    CSeqStatement( const CStatement* _left, const CStatement* _right );
    CSeqStatement( std::unique_ptr<const CStatement> _left, std::unique_ptr<const CStatement> _right );
    ~CSeqStatement();

    const CStatement* LeftStatement() const { return leftStatement.get(); }
    const CStatement* RightStatement() const { return rightStatement.get(); }

    void Accept( IVisitor* visitor ) const override { visitor->Visit( this ); }

private:
    std::unique_ptr<const CStatement> leftStatement;
    std::unique_ptr<const CStatement> rightStatement;
};

//-----------------------------------------------------------------------------------------------//

class CLabelStatement : public CStatement {
public:
    CLabelStatement( CLabel _label );
    ~CLabelStatement();

    CLabel Label() const { return label; }

    void Accept( IVisitor* visitor ) const override { visitor->Visit( this ); }

private:
    CLabel label;
};

} // namespace IRTree
