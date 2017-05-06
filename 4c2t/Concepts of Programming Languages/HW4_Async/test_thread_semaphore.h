#pragma once

#include <chrono>
#include <gtest/gtest.h>
#include <mutex>
#include <thread>

#include "thread_semaphore.h"

class CTestSemaphore : public testing::Test {
public:
    CTestSemaphore() = default;

protected:
    void SetUp() {
        semaphore = std::make_shared<CSemaphore>();
        x = std::make_shared<int>(0);
    }

    void TearDown() {}

    std::shared_ptr<CSemaphore> semaphore;
    std::shared_ptr<int> x;
};

TEST_F(CTestSemaphore, WaitPost) {
    std::shared_ptr<std::mutex> mutex = nullptr;
    auto function = [this, &mutex] () {
        mutex = std::make_shared<std::mutex>();
        std::unique_lock<std::mutex> lock(*mutex);

        semaphore->Wait();
        *x = 1;
    };

    std::thread thread(function);
    while (!mutex) {
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
    EXPECT_EQ(*x, 0);

    semaphore->Post();
    mutex->lock();
    EXPECT_EQ(*x, 1);

    thread.join();
}

TEST_F(CTestSemaphore, GetValue) {
    EXPECT_EQ(semaphore->GetValue(), 0);

    for (int i = 1; i <= 3; ++i) {
        semaphore->Post();
        EXPECT_EQ(semaphore->GetValue(), i);
    }

    for (int i = 2; i >= 0; --i) {
        semaphore->Wait();
        EXPECT_EQ(semaphore->GetValue(), i);
    }
}

TEST_F(CTestSemaphore, TryWait) {
    bool isOpen = semaphore->TryWait();
    EXPECT_EQ(isOpen, false);

    semaphore->Post();

    isOpen = semaphore->TryWait();
    EXPECT_EQ(isOpen, true);
}

