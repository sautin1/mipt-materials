#include "HeapClasses.h"

const size_t CHeapSmall::innerSize = 4;
const size_t CHeapSmallDefault::innerSize = 4;
const size_t CHeapLarge::innerSize = 1 << 20;
const size_t CHeapLargeDefault::innerSize = 1 << 20;

CHeapManager commonHeapManager;
HANDLE defaultHeapHandle;

void InitHeapManager(size_t minSize, size_t maxSize)
{
    commonHeapManager = CHeapManager();
    commonHeapManager.Create(minSize, maxSize);
}

void InitHeapHandle(size_t minSize, size_t maxSize)
{
    defaultHeapHandle = HeapCreate(0, minSize, maxSize);
}

void* CHeapSmall::operator new(size_t size)
{
    return commonHeapManager.Alloc(size * innerSize);
}

void CHeapSmall::operator delete(void* ptr)
{
    return commonHeapManager.Free(ptr);
}

void CHeapSmall::Init(size_t minSize, size_t maxSize)
{
    InitHeapManager(minSize, maxSize);
}

void CHeapSmall::Finalize()
{
    commonHeapManager.Destroy();
}

size_t CHeapSmall::Size()
{
    return innerSize;
}

void* CHeapSmallDefault::operator new(size_t size)
{
    return HeapAlloc(defaultHeapHandle, 0, size * innerSize);
}

void CHeapSmallDefault::operator delete(void* ptr)
{
    HeapFree(defaultHeapHandle, 0, ptr);
}

void CHeapSmallDefault::Init(size_t minSize, size_t maxSize)
{
    InitHeapHandle(minSize, maxSize);
}

void CHeapSmallDefault::Finalize()
{
    HeapDestroy(defaultHeapHandle);
}

size_t CHeapSmallDefault::Size()
{
    return innerSize;
}

void* CHeapLarge::operator new(size_t size){
    return commonHeapManager.Alloc(size * innerSize);
}

void CHeapLarge::operator delete(void* ptr) {
    return commonHeapManager.Free(ptr);
}

void CHeapLarge::Init(size_t minSize, size_t maxSize)
{
    InitHeapManager(minSize, maxSize);
}

void CHeapLarge::Finalize()
{
    commonHeapManager.Destroy();
}

size_t CHeapLarge::Size()
{
    return innerSize;
}

void* CHeapLargeDefault::operator new(size_t size){
    return HeapAlloc(defaultHeapHandle, 0, size * innerSize);
}

void CHeapLargeDefault::operator delete(void* ptr) {
    HeapFree(defaultHeapHandle, 0, ptr);
}

void CHeapLargeDefault::Init(size_t minSize, size_t maxSize)
{
    InitHeapHandle(minSize, maxSize);
}

void CHeapLargeDefault::Finalize()
{
    HeapDestroy(defaultHeapHandle);
}

size_t CHeapLargeDefault::Size()
{
    return innerSize;
}
