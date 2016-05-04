#include "HeapManager.h"

const DWORD CHeapManager::pageSize = CHeapManager::getPageSize();
const std::vector<size_t> CHeapManager::freeBlockMaxSizes = { pageSize - 1, (128 / 4) * pageSize };

int ceilTo(int x, int multiple)
{
    int remainder = x % multiple;
    return x + ((remainder > 0) ? multiple - remainder : 0);
}

CHeapManager::CHeapManager()
{
    for( size_t i = 0; i < freeBlockMaxSizes.size() + 1; ++i ) {
        freeBlockSets.push_back(std::set<BlockInfo>());
    }
}

void CHeapManager::Create(size_t minSize, size_t maxSize)
{
    maxSize = ceilTo(maxSize, pageSize);
    minSize = ceilTo(minSize, pageSize);
    heapMaxSize = maxSize;
    heapPtr = static_cast<byte*>(VirtualAlloc(NULL, maxSize, MEM_RESERVE, PAGE_EXECUTE_READWRITE));
    if( heapPtr == NULL ) {
        std::cerr << "Cannot create heap" << std::endl;
    }
    size_t pageCount = minSize / pageSize;
    pages.resize(pageCount, 1);
    commitPages(heapPtr, minSize);
    addFreeBlock(heapPtr, maxSize);
}

void* CHeapManager::Alloc(size_t requestedSize)
{
    requestedSize += sizeof(size_t);
    byte* blockPtr = static_cast<byte*>(getFreeBlock(requestedSize));
    if( blockPtr == NULL ) {
        std::cerr << "Cannot allocate memory" << std::endl;
        return NULL;
    }
    size_t pageCount = ceilTo(requestedSize, pageSize) / pageSize;
    int startPage = getStartPage(blockPtr);
    commitPages(blockPtr, requestedSize);
    updatePages(blockPtr, pageCount);
    blockPtr = static_cast<byte*>(setSizeLabel(blockPtr, requestedSize));
    allocatedBlocks.insert(blockPtr);
    return blockPtr;
} 

void CHeapManager::Free(void* ptr)
{
    auto it = allocatedBlocks.find(static_cast<byte*>(ptr));
    if( it == allocatedBlocks.end() ) {
        std::cerr << "Cannot find allocated block at pointer " << ptr << "." << std::endl;
        return;
    }
    allocatedBlocks.erase(it);
    ptr = static_cast<byte*>(ptr) - sizeof(size_t);
    size_t blockSize = getSizeLabel(ptr);
    size_t pageCount = ceilTo(blockSize, pageSize) / pageSize;
    int startPage = getStartPage(ptr);
    int endPage = getEndPage(ptr, pageCount);
    int beginFreeRange = -1;
    int endFreeRange = -1;
    for( int i = startPage; i < endPage; ++i ) {
        --pages[i];
        if( pages[i] == 0 ) {
            if( beginFreeRange == -1 ) {
                beginFreeRange = i;
            }
            endFreeRange = i + 1;
        }
    }
    if( beginFreeRange != -1 ) {
        ::VirtualFree(heapPtr + beginFreeRange * pageSize, (endFreeRange - beginFreeRange) * pageSize, MEM_DECOMMIT);
    }

    updateFreeBlocks(ptr, blockSize);
}

void CHeapManager::Destroy()
{
    for( auto it = allocatedBlocks.begin(); it != allocatedBlocks.end(); ++it ) {
        std::cerr << "Allocated block left. Ptr: " << *it << ". Size: " << getSizeLabel(*it) << "." << std::endl;
    }
    VirtualFree(heapPtr, heapMaxSize, MEM_RELEASE);
}

DWORD CHeapManager::getPageSize()
{
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    return info.dwPageSize;
}

void CHeapManager::addFreeBlock(void* blockPtr, size_t blockSize)
{
    int setId = getSetId(blockSize);
    freeBlockSets.at(setId).insert(std::make_pair(static_cast<byte*>(blockPtr), blockSize));
}

void CHeapManager::updateFreeBlocks(void* blockPtr, size_t blockSize)
{
    std::set<BlockInfo>* leftNeighborSet = NULL;
    std::set<BlockInfo>* rightNeighborSet = NULL;
    auto leftNeighbor = freeBlockSets.front().begin();
    auto rightNeighbor = freeBlockSets.back().end();
    for( size_t i = 0; i < freeBlockSets.size(); ++i ) {
        std::set<BlockInfo>& blockSet = freeBlockSets.at(i);
        auto it = blockSet.lower_bound(std::make_pair(static_cast<byte*>(blockPtr), blockSize));
        if( it != blockSet.end() && (rightNeighborSet == NULL || it->first < rightNeighbor->first) ) {
            rightNeighbor = it;
            rightNeighborSet = &blockSet;
        }
        if( it != blockSet.begin() ) {
            --it;
            if( leftNeighborSet == NULL || it->first > leftNeighbor->first ) {
                leftNeighbor = it;
                leftNeighborSet = &blockSet;
            }
        }
    }

    byte* newBlockPtr = static_cast<byte*>(blockPtr);
    size_t newBlockSize = blockSize;
    bool isExtended = false;
    if( leftNeighborSet != NULL && leftNeighbor->first + leftNeighbor->second == blockPtr ) {
        newBlockPtr = leftNeighbor->first;
        newBlockSize += leftNeighbor->second;
        leftNeighborSet->erase(leftNeighbor);
        isExtended = true;
    }
    if( rightNeighborSet != NULL && static_cast<byte*>(blockPtr) + blockSize == rightNeighbor->first ) {
        newBlockSize += rightNeighbor->second;
        rightNeighborSet->erase(rightNeighbor);
        isExtended = true;
    }
    addFreeBlock(newBlockPtr, newBlockSize);
}

void* CHeapManager::getFreeBlock(size_t requestedSize)
{
    void* blockPtr = NULL;
    for( size_t setId = getSetId(requestedSize); setId < freeBlockSets.size(); ++setId ) {
        std::set<BlockInfo>& blockSet = freeBlockSets.at(setId);
        for( auto it = blockSet.begin(); it != blockSet.end(); ++it ) {
            size_t blockSize = it->second;
            if( requestedSize <= blockSize ) {
                blockPtr = it->first;
                blockSet.erase(it);
                void* remainderBlock = static_cast<byte*>(blockPtr) + requestedSize;
                addFreeBlock(remainderBlock, blockSize - requestedSize);
                break;
            }
        }
        if( blockPtr != NULL ) {
            break;
        }
    }
    return blockPtr;
}

void CHeapManager::commitPages(void* memPtr, size_t size)
{
    void* res = ::VirtualAlloc(memPtr, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    if( res == NULL ) {
        std::cerr << "Cannot commit block" << std::endl;
    }
}

void CHeapManager::updatePages(void* memPtr, size_t pageCount)
{
    int startPage = getStartPage(memPtr);
    int endPage = getEndPage(memPtr, pageCount);
    if( endPage > static_cast<int>(pages.size()) ) {
        pages.resize(endPage, 0);
    }
    for( int i = startPage; i < endPage; ++i ) {
        pages[i] += 1;
    }
}

int CHeapManager::getStartPage(void* memPtr)
{
    return (static_cast<byte*>(memPtr) - heapPtr) / pageSize;
}

int CHeapManager::getEndPage(void* memPtr, size_t pageCount)
{
    return getStartPage(memPtr) + pageCount;
}


void* CHeapManager::setSizeLabel(void* memPtr, size_t size)
{
    std::memcpy(memPtr, &size, sizeof(size_t));
    return static_cast<byte*>(memPtr) + sizeof(size_t);
}

size_t CHeapManager::getSizeLabel(void* memPtr) const
{
    size_t res;
    std::memcpy(&res, memPtr, sizeof(size_t));
    return res;
}

int CHeapManager::getSetId(size_t size) const
{
    return std::lower_bound(freeBlockMaxSizes.begin(), freeBlockMaxSizes.end(), size) - freeBlockMaxSizes.begin();
}
