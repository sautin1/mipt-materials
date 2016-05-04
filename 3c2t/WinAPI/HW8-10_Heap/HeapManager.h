#pragma once
#include <algorithm>
#include <iostream>
#include <set>
#include <vector>
#include <Windows.h>

using BlockInfo = std::pair<byte*, size_t>;

int ceilTo(int x, int divider);

class CHeapManager
{
public:
    CHeapManager();
    void Create(size_t minSize, size_t maxSize);
    void* Alloc(size_t size);
    void Free(void* ptr);
    void Destroy();
private:
    static const DWORD pageSize;
    static const std::vector<size_t> freeBlockMaxSizes;
    static DWORD getPageSize();

    void addFreeBlock(void* blockPtr, size_t blockSize);
    void updateFreeBlocks(void* blockPtr, size_t blockSize);
    
    void* getFreeBlock(size_t requestedSize);
    void commitPages(void* memPtr, size_t size);

    void updatePages(void* memPtr, size_t pageCount);
    int getStartPage(void* memPtr);
    int getEndPage(void* memPtr, size_t pageCount);

    void* setSizeLabel(void* memPtr, size_t size);
    size_t getSizeLabel(void* memPtr) const;
    int getSetId(size_t size) const;

    std::vector<std::set<BlockInfo>> freeBlockSets;
    std::set<byte*> allocatedBlocks;
    std::vector<int> pages;

    byte* heapPtr;
    size_t heapMaxSize;
};
