#pragma once

#include <gtest/gtest.h>
#include <memory>
#include <mutex>

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
};

TEST_F(CTestTaskController, Start) {
    std::function<int()> function = []() {
        for (int i = 0; i < 1000000; ++i);
        return 1;
    };
    CTaskController<int> task(threadPool, function);
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
