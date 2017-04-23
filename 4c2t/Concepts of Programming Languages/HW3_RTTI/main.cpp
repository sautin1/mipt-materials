#include <iostream>

#include "RTTI.h"

class MyClass {
public:
    virtual CTypeId GetTypeId() const { return typeId; }
private:
    static const CTypeId typeId;
};

int main() {
    return 0;
}
