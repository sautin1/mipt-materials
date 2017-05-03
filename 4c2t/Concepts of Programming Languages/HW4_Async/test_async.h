#pragma once

#include <functional>
#include <gtest/gtest.h>

#include "async.h"
#include "promise_future.h"
#include "threadpool.h"

class TestAsync : public testing::Test {
public:
    TestAsync() = default;

protected:
    void SetUp() {}
    void TearDown() {}

    CThreadPool threadPool;
};

TEST_F(TestAsync, Sync) {
    int x = 0;
    std::function<int(int)> function = [] (int arg) {
        return arg + 1;
    };

    std::shared_ptr<CFuture<int>> future = Async<int, int>(threadPool, function, x, TTaskLaunchMode::SYNC);
    std::shared_ptr<int> result = future->TryGet();

    EXPECT_NE(result, nullptr);
    EXPECT_EQ(*result, 1);
}

TEST_F(TestAsync, AsyncNotDeferred) {
    int x = 0;
    std::function<int(int)> function = [] (int arg) {
        return arg + 1;
    };

    std::shared_ptr<CFuture<int>> future = Async<int, int>(threadPool, function, x, TTaskLaunchMode::ASYNC);
    std::shared_ptr<int> result = future->Get();
    EXPECT_EQ(*result, 1);
}

TEST_F(TestAsync, AsyncDeferred) {
    int x = 0;
    std::function<int(int)> function = [] (int arg) {
        return arg + 1;
    };

    std::shared_ptr<CFuture<int>> future = Async<int, int>(threadPool, function, x, TTaskLaunchMode::DEFERRED);
    std::shared_ptr<int> result = future->TryGet();
    EXPECT_EQ(result, nullptr);

    result = future->Get();
    EXPECT_NE(result, nullptr);
    EXPECT_EQ(*result, 1);
}
