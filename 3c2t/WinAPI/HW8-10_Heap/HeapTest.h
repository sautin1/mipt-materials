#pragma once
#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <vector>
#include <Windows.h>
#include <Psapi.h> // for memory statistics
#include "gtest/gtest.h"
#include "HeapManager.h"
#include "HeapClasses.h"

template <class HeapTestClass>
void TestHeapAllocFreeSingle(size_t minSize, size_t maxSize, size_t elemCount) {
    HeapTestClass::Init(minSize, maxSize);
    HeapTestClass* ptr = static_cast<HeapTestClass*>(new HeapTestClass[elemCount]);
    delete[] ptr;
    HeapTestClass::Finalize();
}

template <class HeapTestClass>
void TestHeapAllocFreeMultiple(size_t minSize, size_t maxSize, size_t allocCount) {
    HeapTestClass::Init(minSize, maxSize);
    if( HeapTestClass::Size() * allocCount > maxSize ) {
        for( size_t i = 0; i < allocCount; ++i ) {
            HeapTestClass* ptr = static_cast<HeapTestClass*>(new HeapTestClass);
            delete ptr;
        }
    } else {
        std::vector<HeapTestClass*> blocks;
        blocks.reserve(allocCount);
        for( size_t i = 0; i < allocCount; ++i ) {
            blocks.push_back(static_cast<HeapTestClass*>(new HeapTestClass));
        }
        for( size_t i = 0; i < allocCount; ++i ) {
            delete (blocks.back());
            blocks.pop_back();
        }
    }
    HeapTestClass::Finalize();
}

template <class HeapTestClass>
void TestHeapMemoryUsage(size_t minSize, size_t maxSize, size_t allocCount, size_t iterCount) {
    HeapTestClass::Init(minSize, maxSize);

    std::vector<HeapTestClass*> blocks;
    blocks.reserve(allocCount);
    for( unsigned int i = 0; i < allocCount; ++i ) {
        blocks.push_back(static_cast<HeapTestClass*>(new HeapTestClass));
    }
    for( unsigned int iterIndex = 0; iterIndex < iterCount; ++iterIndex ) {
        for( unsigned int allocIndex = 0; allocIndex < allocCount / 2; ++allocIndex ) {
            blocks.push_back(static_cast<HeapTestClass*>(new HeapTestClass));
        }
        std::random_shuffle(blocks.begin(), blocks.end());
        for( unsigned int allocIndex = 0; allocIndex < allocCount / 2; ++allocIndex ) {
            delete blocks.back();
            blocks.pop_back();
        }
    }
    PROCESS_MEMORY_COUNTERS memoryStats;
    ::GetProcessMemoryInfo(::GetCurrentProcess(), &memoryStats, sizeof(PROCESS_MEMORY_COUNTERS));

    HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
    ::SetConsoleTextAttribute(hConsole, 11);
    std::cout << "Memory used: " << memoryStats.WorkingSetSize << std::endl;
    ::SetConsoleTextAttribute(hConsole, 0);

    for( unsigned int i = 0; i < blocks.size(); ++i ) {
        delete blocks[i];
    }

    HeapTestClass::Finalize();
}
