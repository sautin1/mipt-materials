#pragma once

#include <gtest/gtest.h>
#include <functional>
#include <memory>
#include <mutex>

#include "async.h"
#include "promise_future.h"
#include "threadpool.h"


class CTestAsync : public testing::Test {
public:
    CTestAsync() = default;

protected:
    void SetUp() {
        threadPool = std::make_shared<CThreadPool>();
    }

    void TearDown() {}

    std::shared_ptr<CThreadPool> threadPool;
};

TEST_F(CTestAsync, Sync) {
    std::function<int(int)> functionId = [](int value) { return value; };
    std::shared_ptr<CFuture<int>> future = Async<int, int>(threadPool, functionId,
                                                           1, TTaskLaunchMode::SYNC);
    EXPECT_NE(future, nullptr);
    EXPECT_EQ(future->IsFinished(), true);
    std::shared_ptr<int> result = future->TryGet();
    EXPECT_NE(result, nullptr);
    EXPECT_EQ(*result, 1);
}

TEST_F(CTestAsync, Async) {
    std::shared_ptr<std::mutex> mutex(new std::mutex());
    mutex->lock();
    std::function<int(int)> function = [mutex](int value) {
        mutex->lock();
        mutex->unlock();
        return value;
    };

    std::shared_ptr<CFuture<int>> future = Async<int, int>(threadPool, function,
                                                           1, TTaskLaunchMode::ASYNC);
    EXPECT_NE(future, nullptr);
    EXPECT_EQ(future->IsFinished(), false);
    mutex->unlock();

    std::shared_ptr<int> result = future->Get();
    EXPECT_NE(result, nullptr);
    EXPECT_EQ(*result, 1);
}
