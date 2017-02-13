#pragma once

#include <unordered_map>
#include <vector>

#include <IRT/nodes/Expression.h>
#include <IRT/Temporary.h>
#include <IRT/Label.h>

namespace IRTree {

class IAddress {
public:
    virtual ~IAddress() {}
    virtual std::unique_ptr<const CExpression> ToExpression( std::unique_ptr<const CExpression> framePointer ) const = 0;
};

/**
Indicates a memory location at offset X from the frame pointer.
*/
class CAddressInFrame : public IAddress {
public:
    CAddressInFrame( int _offset ) : offset( _offset ) {}

    virtual ~CAddressInFrame() {}
    virtual std::unique_ptr<const CExpression> ToExpression( std::unique_ptr<const CExpression> framePointer ) const override;
protected:
    int offset;
};

class CAddressOfField : public CAddressInFrame {
public:
    CAddressOfField( int _offset ) : CAddressInFrame( _offset ) {}
    std::unique_ptr<const CExpression> ToExpression( std::unique_ptr<const CExpression> thisPointer ) const override;
};

/**
CAddressInRegister (T84) indicates that it will be held in "register" T84
*/
class CAddressInRegister : public IAddress {
public:
    CAddressInRegister( const CTemp& _temp ) : temp( _temp ) {}
    std::unique_ptr<const CExpression> ToExpression( std::unique_ptr<const CExpression> framePointer ) const override;
private:
    CTemp temp;
};

class CFrame {
public:
    // CFrame( CLabel _name ) : name( _name ), maxOffsetFramePointer( 0 ) {}
    CFrame( const std::string& _className, const std::string& _methodName )
        : className( _className ), methodName( _methodName ),
          name( className + "$" + methodName ), maxOffsetFramePointer( 0 ), maxOffsetThis( 0 ) {}

    CTemp FramePointer() const;
    CTemp ReturnValueTemp() const;
    int WordSize() const;

    CLabel GetName() const;
    const std::string& GetClassName() const;
    const std::string& GetMethodName() const;

    // reserves place on stack for method arguments and locals (determines offsets for them)
    void AddThis();
    void AddReturn();
    void AddArgument( const std::string& name );
    void AddLocal( const std::string& name );
    void AddField( const std::string& name );

    const IAddress* GetAddress( const std::string& varName ) const;
    const IAddress* GetThis() const;
    const IAddress* GetReturn() const;

    std::unique_ptr<const CExpression> ExternalCall( const std::string& functionName, std::unique_ptr<const CExpressionList> args ) const;
private:
    int nextOffsetFromFramePointer();
    int nextOffsetFromThis();
    void addAddress( const std::string& name, const IAddress* address );

    std::string className;
    std::string methodName;
    CLabel name;
    std::unordered_map<std::string, std::unique_ptr<const IAddress>> addresses;

    static const int wordSize;
    static const std::string thisName;
    static const std::string returnName; // return address

    static const CTemp returnValueTemp; // return value
    static const CTemp framePointerTemp;

    int maxOffsetFramePointer;
    int maxOffsetThis;
};


}