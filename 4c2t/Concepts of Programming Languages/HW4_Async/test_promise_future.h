#pragma once

#include <chrono>
#include <gtest/gtest.h>
#include <stdexcept>
#include <string>

#include "promise_future.h"
#include "threadpool.h"

class TestPromiseFuture : public testing::Test {
public:
    TestPromiseFuture() = default;

protected:
    void SetUp() {
        promise = std::shared_ptr<CPromise<int>>(new CPromise<int>());
        future = promise->GetFuture();
    }

    void TearDown() {}

    std::shared_ptr<CPromise<int>> promise;
    std::shared_ptr<CFuture<int>> future;
};

TEST_F(TestPromiseFuture, WaitForResult) {
    auto work = [this](int workDuration) {
        std::this_thread::sleep_for(std::chrono::milliseconds(workDuration));
        promise->SetValue(23);
    };
    std::thread thread(work, 50);
    thread.detach();

    EXPECT_EQ(future->TryGet(), nullptr);
    int result = *(future->Get());
    EXPECT_EQ(result, 23);
    EXPECT_EQ(result, *(future->TryGet()));
}

TEST_F(TestPromiseFuture, ExceptionInPromise) {
    std::string error_message = "It's a trap!";
    std::logic_error error(error_message);
    auto work = [this, error](int workDuration) {
        std::this_thread::sleep_for(std::chrono::milliseconds(workDuration));
        promise->SetException(error);
    };
    std::thread thread(work, 50);
    thread.detach();

    try {
        *(future->Get());
        FAIL() << "Expected std::logic_error";
    } catch (const std::logic_error& err) {
        EXPECT_EQ(err.what(), error_message);
    } catch (...) {
        FAIL() << "Expected std::logic_error";
    }
}

TEST_F(TestPromiseFuture, WaitMethod) {
    auto work = [this](int workDuration) {
        std::this_thread::sleep_for(std::chrono::milliseconds(workDuration));
        promise->SetValue(46);
    };
    std::thread thread(work, 50);
    thread.detach();

    EXPECT_EQ(future->TryGet(), nullptr);
    future->Wait();
    std::shared_ptr<int> result = future->TryGet();
    EXPECT_NE(result, nullptr);
    EXPECT_EQ(*result, 46);
}

TEST_F(TestPromiseFuture, ThenInt) {
    CThreadPool threadPool;
    std::function<int(int)> incFunction = [](int x) {
        return x + 1;
    };
    std::shared_ptr<CFuture<int>> futureStart = Async<int, int>(threadPool,
                                                                incFunction,
                                                                0,
                                                                TTaskLaunchMode::DEFERRED);

    std::shared_ptr<CFuture<int>> futureCombined = futureStart
            ->Then(threadPool, incFunction)
            ->Then(threadPool, incFunction);
    try {
        std::shared_ptr<int> result = futureCombined->Get();
        EXPECT_NE(result, nullptr);
        EXPECT_EQ(*result, 3);
    } catch (const std::exception& error) {
        FAIL() << "No exception was expected";
    }
}
