#pragma once
#include <memory>
#include <unordered_map>

class CClassInfo {
public:
    CClassInfo(int id, std::string name) : classId(id), className(name) {}
    int GetClassId() { return classId; }
    std::string GetClassName() { return className; }
private:
    const int classId;
    const std::string className;
    std::vector<std::unique_ptr<CClassInfo>> derivedClasses;
    std::vector<std::unique_ptr<CClassInfo>> baseClasses;
};

class CClassHierarchy {
public:
    CClassHierarchy() : classCount(0) {}
    void AddClass() {}
private:
    int classCount;
    std::unordered_map<int, std::unique_ptr<CClassInfo>> classIdToInfo;
};
