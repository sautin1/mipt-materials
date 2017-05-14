#pragma once

#include <algorithm>
#include <gtest/gtest.h>
#include <memory>
#include <mutex>
#include <string>
#include <stdexcept>
#include <sstream>
#include <vector>

#include "task_controller.h"
#include "threadpool.h"

class CTestTaskController : public testing::Test {
public:
    CTestTaskController() = default;

protected:
    void SetUp() {
        threadPool = std::make_shared<CThreadPool>();
        createStringFunctions();
        createArithmeticFunctions();
    }

    void TearDown() {}

    std::shared_ptr<CThreadPool> threadPool;

    // string functions
    std::function<std::string()> functionPangram;
    std::function<std::string()> functionQuote;
    std::function<int(const std::string&)> functionLength;
    std::function<std::vector<std::string>(const std::string&)> functionSplit;
    std::function<std::vector<std::string>(const std::vector<std::string>)> functionFilterEmpty;
    std::function<std::vector<int>(const std::vector<std::string>&)> functionLineLengths;
    std::function<std::vector<int>(std::vector<int>)> functionSort;
    std::function<std::vector<int>(std::vector<int>)> functionReverse;
    std::function<int(const std::vector<int>&)> functionTakeFront;

    // id functions
    std::function<int()> functionZero;
    std::function<int(int)> functionIncrement;
    std::function<int(int)> functionIdWithCounter;
    std::function<int(int)> functionIdWithExceptionThrown;

    std::string exceptionMessage;
    std::string pangram;
    std::string quote;
    std::shared_ptr<int> functionCallCounter;

    void createStringFunctions();
    void createArithmeticFunctions();
};

void CTestTaskController::createStringFunctions() {
    pangram = "The quick brown fox jumps over the lazy dog";
    functionPangram = [this]() {
        return pangram;
    };

    quote = "There are only two kinds of languages : the ones people complain about and the ones nobody uses.";
    functionQuote = [this]() {
        return quote;
    };

    functionLength = [](const std::string& line) {
        return line.size();
    };

    functionSplit = [](const std::string& line) {
        std::stringstream sstream(line);
        std::vector<std::string> words;
        while (sstream) {
            std::string word;
            sstream >> word;
            words.push_back(word);
        }
        return words;
    };

    functionFilterEmpty = [](const std::vector<std::string>& lines) {
        std::vector<std::string> linesFiltered(lines.size());
        auto it = std::copy_if(lines.begin(), lines.end(), linesFiltered.begin(), [](const std::string& line) {
            return !line.empty();
        });
        linesFiltered.resize(std::distance(linesFiltered.begin(), it));
        return linesFiltered;
    };

    functionLineLengths = [](const std::vector<std::string>& lines) {
        std::vector<int> lengths(lines.size());
        std::transform(lines.begin(), lines.end(), lengths.begin(), [](const std::string& line) {
            return line.size();
        });
        return lengths;
    };

    functionSort = [](std::vector<int> numbers) {
        std::sort(numbers.begin(), numbers.end());
        return numbers;
    };

    functionReverse = [](std::vector<int> numbers) {
        std::reverse(numbers.begin(), numbers.end());
        return numbers;
    };

    functionTakeFront = [](const std::vector<int>& numbers) {
        return numbers.front();
    };
}

void CTestTaskController::createArithmeticFunctions() {
    functionCallCounter = std::make_shared<int>(0);

    functionZero = []() { return 0; };
    functionIncrement = [](int x) { return x + 1; };
    functionIdWithCounter = [this](int x) {
        (*functionCallCounter)++;
        return x;
    };
    exceptionMessage = "Test";
    functionIdWithExceptionThrown = [this](int x) {
        throw std::logic_error(exceptionMessage);
        return x;
    };
}

TEST_F(CTestTaskController, Start) {
    std::function<int()> function = []() {
        for (int i = 0; i < 1000000; ++i);
        return 1;
    };
    CTaskController<int> task(threadPool, function);
    EXPECT_EQ(task.IsCompleted(), false);
    task.Start();
    EXPECT_EQ(task.IsCompleted(), true);
    std::shared_ptr<int> result = task.GetFuture()->TryGet();
    EXPECT_NE(result, nullptr);
    EXPECT_EQ(*result, 1);
}

TEST_F(CTestTaskController, Delegate) {
    std::shared_ptr<std::mutex> mutex(new std::mutex());
    mutex->lock();
    std::function<int()> function = [mutex]() {
        mutex->lock();
        mutex->unlock();
        return 1;
    };
    CTaskController<int> task(threadPool, function);
    task.Delegate();
    EXPECT_EQ(task.IsCompleted(), false);

    mutex->unlock();
    task.WaitCompleted();
    EXPECT_EQ(task.IsCompleted(), true);

    std::shared_ptr<int> result = task.GetFuture()->TryGet();
    EXPECT_NE(result, nullptr);
    EXPECT_EQ(*result, 1);
}

TEST_F(CTestTaskController, ThenChainLineLength) {
    std::shared_ptr<CTaskController<std::string>> taskPangram(
        new CTaskController<std::string>(threadPool, functionPangram)
    );
    std::shared_ptr<CTaskController<int>> taskLength = Then<std::string, int>(taskPangram, functionLength);

    EXPECT_NE(taskLength, nullptr);
    taskLength->Delegate();
    std::shared_ptr<int> result = taskLength->GetFuture()->Get();
    EXPECT_NE(result, nullptr);
    EXPECT_EQ(*result, pangram.length());
}

TEST_F(CTestTaskController, ThenChainInc) {
    std::shared_ptr<CTaskController<int>> taskZero(
        new CTaskController<int>(threadPool, functionZero)
    );

    std::shared_ptr<CTaskController<int>> taskInc = Then<int, int>(
        Then<int, int>(taskZero, functionIncrement),
        functionIncrement
    );
    EXPECT_NE(taskInc, nullptr);

    taskInc->Delegate();
    std::shared_ptr<int> result = taskInc->GetFuture()->Get();
    EXPECT_NE(result, nullptr);
    EXPECT_EQ(*result, 2);
}

TEST_F(CTestTaskController, ThenChainLongestWordLength) {
    std::shared_ptr<CTaskController<std::string>> taskQuote(
        new CTaskController<std::string>(threadPool, functionQuote)
    );
    std::shared_ptr<CTaskController<int>> taskWordMaxLength;
    taskWordMaxLength = Then<std::vector<int>, int>(
        Then<std::vector<int>, std::vector<int>>(
            Then<std::vector<int>, std::vector<int>>(
                Then<std::vector<std::string>, std::vector<int>>(
                    Then<std::vector<std::string>, std::vector<std::string>>(
                        Then<std::string, std::vector<std::string>>(
                            taskQuote,
                            functionSplit
                        ),
                        functionFilterEmpty
                    ),
                    functionLineLengths
                ),
                functionSort
            ),
            functionReverse
        ),
        functionTakeFront
    );
    EXPECT_NE(taskWordMaxLength, nullptr);

    taskWordMaxLength->Delegate();
    std::shared_ptr<int> result = taskWordMaxLength->GetFuture()->Get();
    EXPECT_NE(result, nullptr);
    EXPECT_EQ(*result, std::string("languages").size());
}

TEST_F(CTestTaskController, ThenChainWithException) {
    std::shared_ptr<CTaskController<int>> taskFunctionCallCounter(
        new CTaskController<int>(threadPool, functionZero)
    );
    taskFunctionCallCounter = Then<int, int>(
        Then<int, int>(
            Then<int, int>(
                taskFunctionCallCounter,
                functionIdWithCounter
            ),
            functionIdWithExceptionThrown
        ),
        functionIdWithCounter
    );

    taskFunctionCallCounter->Start();

    try {
        taskFunctionCallCounter->GetFuture()->Get();
        FAIL() << "CAsyncException expected";
    } catch (const CAsyncException& err) {
        EXPECT_EQ(err.what(), exceptionMessage);
    } catch (...) {
        FAIL() << "CAsyncException expected";
    }

    // check that functionIncrement was called only once
    EXPECT_EQ(*functionCallCounter, 1);
}

TEST_F(CTestTaskController, ThenChainAddChainLinkAfterStart) {
    std::shared_ptr<CTaskController<int>> taskFunctionCallCounter(
        new CTaskController<int>(threadPool, functionZero)
    );
    taskFunctionCallCounter = Then<int, int>(
        Then<int, int>(
            Then<int, int>(
                taskFunctionCallCounter,
                functionIdWithCounter
            ),
            functionIdWithCounter
        ),
        functionIdWithCounter
    );

    taskFunctionCallCounter->Start();
    EXPECT_EQ(*functionCallCounter, 3);

    std::shared_ptr<CTaskController<int>> taskFunctionCallCounterExtended = Then<int, int>(
        taskFunctionCallCounter,
        functionIdWithCounter
    );

    taskFunctionCallCounterExtended->Start();
    // check that first three functionIdWithCounter are not called once more
    EXPECT_EQ(*functionCallCounter, 4);

    // check that the intermediate result is still accessible
    EXPECT_EQ(*(taskFunctionCallCounter->GetFuture()->TryGet()), 0);
}
