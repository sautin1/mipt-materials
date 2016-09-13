// Author: Alexey Zhuravlev
// Description: Expression Interface and it's implementations

#include <Visitor.h>
#include <VisitorTarget.h>
#include <string>

class IExpression : public IVisitorTarget {
};

//-----------------------------------------------------------------------------------------------//

struct CBinaryExpression: public IExpression {

    enum TOperandType {
        OT_Plus,
        OT_Minus,
        OT_Times,
        OT_Div,

        OT_Count
    };

    TOperandType Operation;

    IExpression* LeftOperand, RightOperand;

    void Accept( IVisitor* visitor ) override { visitor->Visit( this ); }
};

//-----------------------------------------------------------------------------------------------//

struct CNumberExpression : public IExpression {

    int Value;

    void Accept( IVisitor* visitor ) override { visitor->Visit( this ); }
};

//----------------------------------------------------------------------------------------------//

// Expression for identities (variables)
struct CIdExpression : public IExpression {

    std::string Name; // name of the variable

    void Accept( IVisitor* visitor ) override { visitor->Visit( this ); }
};

//-----------------------------------------------------------------------------------------------//

// Interface for list expressions
class IListExpression : public IExpression {
};

//-----------------------------------------------------------------------------------------------//

// Expression for list of more than two elements
struct CPairListExpression : public IListExpression {

    IExpression* Head; // First element of the list
    IListExpression* Tail; // All other elements

    void Accept( IVisitor* visitor ) override { visitor->Visit( this ); }
};

//-----------------------------------------------------------------------------------------------//

// Expression for list of single element
struct CSingleElementListExpression : public IListExpression {

    IExpression* Element;

    void Accept( IVisitor* visitor ) override { visitor->Visit( this ); }
};
