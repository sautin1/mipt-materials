#pragma once

#include <gtest/gtest.h>
#include <vector>

#include "promise_future.h"
#include "threadpool.h"

class TestThreadPool : public testing::Test {
public:
    TestThreadPool() {}

protected:
    void SetUp() {
        threadPool = std::make_shared<CThreadPool>();
    }

    void TearDown() {}

    std::shared_ptr<CThreadPool> threadPool;
};

TEST_F(TestThreadPool, AddTasks) {
    int size = 100;
    std::vector<std::shared_ptr<CFuture<int>>> futures;
    futures.reserve(size);
    for (int i = 0; i < size; ++i) {
        std::shared_ptr<CPromise<int>> promise(new CPromise<int>());
        futures.emplace_back(promise->GetFuture());
        int arg = i;
        CProcedure procedure = [promise, arg]() {
            for (int i = 0; i < arg * 10000; ++i);
            promise->SetValue(arg * arg);
        };
        threadPool->AddTask(procedure);
    }

    for (int i = 0; i < size; ++i) {
        std::shared_ptr<int> result = futures[i]->Get();
        EXPECT_NE(result, nullptr);
        EXPECT_EQ(*result, i * i);
    }
}
