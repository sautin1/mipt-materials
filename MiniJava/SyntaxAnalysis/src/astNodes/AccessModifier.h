#pragma once

#include <VisitorTarget.h>
#include <Visitor.h>

class CAccessModifier : public CVisitorTarget {
public:
    CAccessModifier( const CLocation& _location ) : CVisitorTarget( _location ) {}
    virtual ~CAccessModifier() {}
};

class CPublicAccessModifier : public CAccessModifier {
public:
    CPublicAccessModifier( const CLocation& _location ) : CAccessModifier( _location ) {}
    void Accept( IVisitor* visitor ) const override { visitor->Visit( this ); }
};

class CPrivateAccessModifier : public CAccessModifier {
public:
    CPrivateAccessModifier( const CLocation& _location ) : CAccessModifier( _location ) {}
    void Accept( IVisitor* visitor ) const override { visitor->Visit( this ); }
};
