#pragma once

#include <algorithm>
#include <gtest/gtest.h>
#include <memory>
#include <mutex>
#include <string>
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
    }

    void TearDown() {}

    std::shared_ptr<CThreadPool> threadPool;

    // functions
    std::function<std::string()> functionQuote;
    std::function<std::vector<std::string>(const std::string&)> functionSplit;
    std::function<std::vector<std::string>(const std::vector<std::string>)> functionFilterEmpty;
    std::function<std::vector<int>(const std::vector<std::string>&)> functionLineLengths;
    std::function<std::vector<int>(std::vector<int>)> functionSort;
    std::function<std::vector<int>(std::vector<int>)> functionReverse;
    std::function<int(const std::vector<int>&)> functionTakeFront;

    void createFunctions();
};

void CTestTaskController::createFunctions() {
    functionQuote = []() {
            return "There are only two kinds of languages : the ones people complain about and the ones nobody uses.";
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
    std::string line = "The quick brown fox jumps over the lazy dog";
    std::function<std::string()> functionPangram = [line]() {
        return line;
    };

    std::function<int(std::string)> functionLength = [](const std::string& line) {
        return line.size();
    };

    CTaskController<std::string> taskPangram(threadPool, functionPangram);
    CTaskController<int> taskLength = taskPangram.Then<int>(functionLength);

    taskLength.Delegate();
    std::shared_ptr<int> result = taskLength.GetFuture()->Get();
    EXPECT_NE(result, nullptr);
    EXPECT_EQ(*result, line.length());
}

TEST_F(CTestTaskController, ThenChainLongestWordLength) {
    createFunctions();
    CTaskController<std::string> taskQuote(threadPool, functionQuote);
    CTaskController<int> taskWordMaxLength = taskQuote
            .Then<std::vector<std::string>>(functionSplit)
            .Then<std::vector<std::string>>(functionFilterEmpty)
            .Then<std::vector<int>>(functionLineLengths)
            .Then<std::vector<int>>(functionSort)
            .Then<std::vector<int>>(functionReverse)
            .Then<int>(functionTakeFront);

    taskWordMaxLength.Delegate();
    std::shared_ptr<int> result = taskWordMaxLength.GetFuture()->Get();
    EXPECT_NE(result, nullptr);
    EXPECT_EQ(*result, std::string("languages").size());
}
