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

TEST_F(TestAsync, NotDeferred) {
    int x = 0;
    std::function<int(int)> function = [] (int arg) {
        return arg + 1;
    };

    std::shared_ptr<CFuture<int>> future = Async<int, int>(false, threadPool, function, x);
    std::shared_ptr<int> result = future->Get();
    EXPECT_EQ(*result, 1);
}
