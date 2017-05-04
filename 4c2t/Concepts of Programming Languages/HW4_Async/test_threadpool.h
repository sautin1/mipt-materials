#pragma once

#include "threadpool.h"

class TestThreadPool : public testing::Test {
public:
    TestThreadPool() {}

protected:
    void SetUp() {
        threadPool = std::make_shared<CThreadPool>();
        x = std::make_shared<int>(0);
    }

    void TearDown() {}

    std::shared_ptr<CThreadPool> threadPool;
    std::shared_ptr<int> x;
};

TEST_F(TestThreadPool, AddTask) {
    CProcedure procedure = [this]() {
        *x = 1;
    };

    threadPool->AddTask(procedure);
}
