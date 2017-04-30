#pragma once

#include <atomic>
#include <chrono>
#include <functional>
#include <gtest/gtest.h>
#include <iostream>
#include <memory>
#include "threadpool.h"

TEST(TestPackedTask, DISABLED_Start) {
//    std::shared_ptr<int> x = std::make_shared<int>(0);
//    auto procedure = [x]() {
//        *x = 1;
//    };
//    CPackedTask task(procedure, std::make_shared<std::atomic<bool>>(true));
//    EXPECT_EQ(*x, 0);
//    task.Start();
//    EXPECT_EQ(*x, 1);
}

TEST(TestPackedTask, DISABLED_ShouldStart) {
//    std::shared_ptr<int> x = std::make_shared<int>(0);
//    auto procedure = [x]() {
//        *x = 1;
//    };
//    CPackedTask task(procedure, std::make_shared<std::atomic<bool>>(false));
//    EXPECT_EQ(task.ShouldStart(), false);
}

TEST(TestThreadPool, AddTaskAndStart) {
    try {
        CThreadPool threadPool;
        std::shared_ptr<int> x = std::make_shared<int>(0);
        std::function<void()> procedure = [x]() {
            *x = 1;
        };
        std::shared_ptr<std::atomic<bool>> shouldStart(new std::atomic<bool>(true));
//        std::function<void()> f = []() -> void { std::cout << "Lol\n"; };
        CPackedTask task(procedure, shouldStart);
//        std::cout << "AA###AA\n";

//        threadPool.AddTask(task);
//        std::this_thread::sleep_for(std::chrono::milliseconds(50));
//        EXPECT_EQ(*x, 0);
    } catch (std::exception error) {
        std::cout << "Caught error: " << error.what();
        throw;
    }
}

TEST(TestThreadPool, DISABLED_AddTaskNoStart) {
//    CThreadPool threadPool;
//    std::shared_ptr<int> x = std::make_shared<int>(0);
//    auto procedure = [x]() {
//        *x = 1;
//    };
//    CPackedTask task(procedure, std::make_shared<std::atomic<bool>>(false));
//    threadPool.AddTask(task);
//    std::this_thread::sleep_for(std::chrono::milliseconds(50));
//    EXPECT_EQ(*x, 0);
}
