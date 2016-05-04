#include "HeapTest.h"

// Manual tests

TEST(ManualTest, Pretest)
{
    CHeapManager heapManager;
    heapManager.Create(1 << 20, 1 << 25);
    std::vector<void*> blocks;
    blocks.push_back(heapManager.Alloc(4));
    blocks.push_back(heapManager.Alloc(5));
    blocks.push_back(heapManager.Alloc(6));
    for( unsigned int i = 0; i < blocks.size(); ++i ) {
        heapManager.Free(blocks[i]);
    }

    blocks.push_back(heapManager.Alloc(7));
    heapManager.Free(blocks.back());
    blocks.pop_back();
    blocks.push_back(heapManager.Alloc(8));
    heapManager.Free(blocks.back());
    blocks.pop_back();
    blocks.push_back(heapManager.Alloc(9));
    heapManager.Free(blocks.back());
    blocks.pop_back();

    heapManager.Destroy();
}

TEST(ManualTest, NumbersSeparate)
{
    CHeapManager heapManager;
    heapManager.Create(1 << 20, 1 << 25);
    std::vector<int*> numbers;
    int maxNumbers = 10000;
    for( int i = 0; i < maxNumbers; ++i ) {
        numbers.push_back(static_cast<int*>(heapManager.Alloc(sizeof(int))));
        ASSERT_TRUE(i == 0 || numbers[i - 1] + 2 == numbers[i]); // adding 2 * sizeof(int)
        *(numbers[i]) = i;
    }
    for( unsigned int i = 0; i < numbers.size(); ++i ) {
        ASSERT_EQ(*(numbers[i]), i);
    }
    int* numbersBeginPtr = numbers.front();
    std::random_shuffle(numbers.begin(), numbers.end());
    for( int i = 0; i < maxNumbers / 2; ++i ) {
        heapManager.Free(static_cast<void*>(numbers.back()));
        numbers.pop_back();
    }
    for( unsigned int i = 0; i < numbers.size(); ++i ) {
        ASSERT_EQ(numbers[i] - numbersBeginPtr, *(numbers[i]) * 2);
        heapManager.Free(static_cast<void*>(numbers[i]));
    }
    heapManager.Destroy();
}

TEST(ManualTest, NumbersGroup)
{
    CHeapManager heapManager;
    heapManager.Create(1 << 20, 1 << 25);
    int maxNumbers = 10000;
    int* numbers = static_cast<int*>(heapManager.Alloc(maxNumbers * sizeof(int)));
    for( int i = 0; i < maxNumbers / 2; ++i ) {
        numbers[i] = i;
    }
    memcpy(numbers + maxNumbers / 2, numbers, (maxNumbers / 2) * sizeof(int));
    for( int i = 0; i < maxNumbers; ++i ) {
        ASSERT_EQ(numbers[i], i % (maxNumbers / 2));
    }
    heapManager.Free(numbers);
    heapManager.Destroy();
}

TEST(ManualTest, BigBlocks)
{
    CHeapManager heapManager;
    heapManager.Create(1 << 10, 1 << 25);
    std::vector<int*> blocks;
    int maxNumbers = 1 << 18;
    for( int i = 0; i < 10; ++i ) {
        blocks.push_back(static_cast<int*>(heapManager.Alloc(maxNumbers * sizeof(int))));
        for( int j = 0; j < maxNumbers; ++j ) {
            blocks[i][j] = i;
        }
    }
    for( unsigned int i = 0; i < blocks.size(); ++i ) {
        bool res = std::all_of(blocks[i], blocks[i] + maxNumbers, [i](int num) { return num == i; });
        ASSERT_TRUE(res);
    }

    for( unsigned int i = 1; i < blocks.size() - 1; ++i ) {
        heapManager.Free(blocks[i]);
    }
    ASSERT_TRUE(std::all_of(blocks.front(), blocks.front() + maxNumbers, [](int num) { return num == 0; }));
    ASSERT_TRUE(std::all_of(blocks.back(), blocks.back() + maxNumbers, [&blocks](int num) { return num == blocks.size() - 1; }));
    heapManager.Free(blocks.front());
    heapManager.Free(blocks.back());
    heapManager.Destroy();
}

// Class tests

TEST(ClassTest, CHeapSmallTest)
{
    TestHeapAllocFreeSingle<CHeapSmall>(1 << 12, 1 << 20, 50);
}

TEST(ClassTest, CHeapSmallDefaultTest)
{
    TestHeapAllocFreeSingle<CHeapSmallDefault>(1 << 12, 1 << 20, 50);
}

TEST(ClassTest, CHeapLargeTest)
{
    TestHeapAllocFreeSingle<CHeapLarge>(1 << 12, 1 << 20, 50);
}

TEST(ClassTest, CHeapLargeDefaultTest)
{
    TestHeapAllocFreeSingle<CHeapLargeDefault>(1 << 12, 1 << 20, 50);
}

// Time tests

TEST(TimeTestSmallBlocks, CHeapSmallTest)
{
    TestHeapAllocFreeMultiple<CHeapSmall>(1 << 10, 1 << 25, 100000);
}

TEST(TimeTestSmallBlocks, CHeapSmallDefaultTest)
{
    TestHeapAllocFreeMultiple<CHeapSmallDefault>(1 << 10, 1 << 25, 100000);
}

TEST(TimeTestLargeBlocks, CHeapLargeTest)
{
    TestHeapAllocFreeMultiple<CHeapLarge>(1 << 10, 1 << 25, 100000);
}

TEST(TimeTestLargeBlocks, CHeapLargeDefaultTest)
{
    TestHeapAllocFreeMultiple<CHeapLargeDefault>(1 << 10, 1 << 25, 100000);
}


// Memory tests

TEST(MemoryTestSmallBlocks, CHeapSmallTest)
{
    TestHeapMemoryUsage<CHeapSmall>(1 << 10, 1 << 25, 1000, 100);
}

TEST(MemoryTestSmallBlocks, CHeapSmallDefaultTest)
{
    TestHeapMemoryUsage<CHeapSmallDefault>(1 << 10, 1 << 25, 1000, 100);
}
