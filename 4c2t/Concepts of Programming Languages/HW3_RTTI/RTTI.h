#pragma once
#include <memory>
#include <unordered_map>
#include <memory>

class CTypeInfo {
public:
    CTypeInfo(std::string name) : typeCode(typeCount++), typeName(name) {}
    int GetTypeId() { return typeCode; }
    std::string GetTypeName() { return typeName; }
private:
    const int typeCode;
    const std::string typeName;
    static int typeCount;
};

class CTypeId {
public:
    CTypeId() = default;

    int GetTypeId() { return info->GetTypeId(); }
    std::string GetTypeName() { return info->GetTypeName(); }
    bool operator ==(const CTypeId& other) { return other.info == info; }
private:
    std::shared_ptr<CTypeInfo> info;
};
