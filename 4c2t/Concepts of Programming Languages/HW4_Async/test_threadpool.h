#pragma once

#include <atomic>
#include <chrono>
#include <functional>
#include <gtest/gtest.h>
#include <iostream>
#include <memory>
#include "threadpool.h"

class TestPackedTask : public testing::Test {
public:
    TestPackedTask()
        : baseProcedure([](std::shared_ptr<int> x) { *x = 1; }) {}

protected:
    void SetUp() {
        x = std::make_shared<int>(0);
        procedure = std::bind(baseProcedure, x);
    }

    void TearDown() {}

    std::shared_ptr<int> x;
    std::function<void(std::shared_ptr<int>)> baseProcedure;
    std::function<void()> procedure;
};

TEST_F(TestPackedTask, Start) {
    CPackedTask task(procedure, std::make_shared<std::atomic<bool>>(true));
    EXPECT_EQ(*x, 0);
    task.Start(); // synchronous
    EXPECT_EQ(*x, 1);
}

TEST_F(TestPackedTask, ShouldStart) {
    CPackedTask task(procedure, std::make_shared<std::atomic<bool>>(false));
    EXPECT_EQ(task.ShouldStart(), false);
}


class TestThreadPool: public testing::Test {
public:
    TestThreadPool()
        : baseProcedure([](std::shared_ptr<std::atomic<int>> x) { x->store(1); }) {}


protected:
    void SetUp() {
        x = std::make_shared<std::atomic<int>>(0);
        procedure = std::bind(baseProcedure, x);
    }

    void TearDown() {}

    void waitForValue(std::shared_ptr<std::atomic<int>> x, int value, int iter = 10, int sleepMs = 50) {
        for (int i = 0; i < iter; ++i) {
            std::this_thread::sleep_for(std::chrono::milliseconds(sleepMs));
            if (x->load() == value) {
                break;
            }
        }
    }

    CThreadPool threadPool;
    std::shared_ptr<std::atomic<int>> x;
    std::function<void(std::shared_ptr<std::atomic<int>>)> baseProcedure;
    std::function<void()> procedure;
};

TEST_F(TestThreadPool, AddTaskAndStart) {
    std::shared_ptr<std::atomic<bool>> shouldStart(new std::atomic<bool>(true));
    CPackedTask task(procedure, shouldStart);
    threadPool.AddTask(task);

    waitForValue(x, 1);

    EXPECT_EQ(x->load(), 1);
}

TEST_F(TestThreadPool, AddTaskAndStartLater) {
    int readyThreadCount = threadPool.ReadyThreadCount();
    std::shared_ptr<std::atomic<bool>> shouldStart(new std::atomic<bool>(false));
    CPackedTask task(procedure, shouldStart);
    threadPool.AddTask(task);
    EXPECT_EQ(threadPool.ReadyThreadCount(), readyThreadCount);

    std::this_thread::sleep_for(std::chrono::milliseconds(50));
    EXPECT_EQ(x->load(), 0);
    shouldStart->store(true);

    waitForValue(x, 1);

    EXPECT_EQ(x->load(), 1);
}
