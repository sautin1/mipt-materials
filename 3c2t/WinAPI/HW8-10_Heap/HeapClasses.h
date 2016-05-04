#pragma once
#include "HeapManager.h"

extern CHeapManager commonHeapManager;
extern HANDLE defaultHeapHandle;

void InitHeapManager(size_t minSize, size_t maxSize);
void InitHeapHandle(size_t minSize, size_t maxSize);

class CHeapSmall {
public:
    static void* operator new(size_t size);
    static void operator delete(void* ptr);
    static void Init(size_t minSize, size_t maxSize);
    static void Finalize();
    static size_t Size();
private:
    static const size_t innerSize;
};

class CHeapSmallDefault {
public:
    static void* operator new(size_t size);
    static void operator delete(void* ptr);
    static void Init(size_t minSize, size_t maxSize);
    static void Finalize();
    static size_t Size();
private:
    static const size_t innerSize;
};

class CHeapLarge {
public:
    static void* operator new(size_t size);
    static void operator delete(void* ptr);
    static void Init(size_t minSize, size_t maxSize);
    static void Finalize();
    static size_t Size();
private:
    static const size_t innerSize;
};

class CHeapLargeDefault {
public:
    static void* operator new(size_t size);
    static void operator delete(void* ptr);
    static void Init(size_t minSize, size_t maxSize);
    static void Finalize();
    static size_t Size();
private:
    static const size_t innerSize;
};
