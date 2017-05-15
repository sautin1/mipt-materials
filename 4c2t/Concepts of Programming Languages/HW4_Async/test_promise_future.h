#pragma once

#include <gtest/gtest.h>
#include <stdexcept>
#include <string>

#include "promise_future.h"


class CTestPromiseFuture : public testing::Test {
public:
    CTestPromiseFuture() = default;

protected:
    void SetUp() {
        promise = std::make_shared<CPromise<int>>();
        future = promise->GetFuture();
    }

    void TearDown() {}

    std::shared_ptr<CPromise<int>> promise;
    std::shared_ptr<CFuture<int>> future;
};

TEST_F(CTestPromiseFuture, WaitForResult) {
    std::shared_ptr<std::mutex> mutex(new std::mutex());
    mutex->lock();
    auto work = [this, mutex]() {
        mutex->lock();
        promise->SetValue(23);
        mutex->unlock();
    };
    std::thread thread(work);

    EXPECT_EQ(future->TryGet(), nullptr);
    mutex->unlock();

    future->Wait();

    std::shared_ptr<int> result = future->Get();
    EXPECT_NE(result, nullptr);
    EXPECT_EQ(*result, 23);

    EXPECT_EQ(future->IsFinished(), true);
    std::shared_ptr<int> resultNew = future->TryGet();
    EXPECT_NE(resultNew, nullptr);
    EXPECT_EQ(*result, *resultNew);

    thread.join();
}

TEST_F(CTestPromiseFuture, ExceptionInPromise) {
    std::string errorMessage = "It's a trap!";
    auto work = [this, &errorMessage]() {
        std::logic_error error(errorMessage);
        promise->SetException(error);
    };
    std::thread thread(work);

    try {
        future->Get();
        FAIL() << "CAsyncException expected";
    } catch (const CAsyncException& err) {
        EXPECT_EQ(err.what(), errorMessage);
    } catch (...) {
        FAIL() << "CAsyncException expected";
    }

    thread.join();
}

TEST_F(CTestPromiseFuture, MultipleFutures) {
    std::shared_ptr<std::mutex> mutex(new std::mutex());
    mutex->lock();
    auto work = [this, mutex]() {
        mutex->lock();
        promise->SetValue(23);
        mutex->unlock();
    };
    std::thread thread(work);

    std::shared_ptr<CFuture<int>> futureOther = promise->GetFuture();
    EXPECT_EQ(future->TryGet(), nullptr);
    EXPECT_EQ(futureOther->TryGet(), nullptr);
    mutex->unlock();

    std::shared_ptr<int> result = future->Get();
    std::shared_ptr<int> resultOther = futureOther->Get();
    EXPECT_NE(result, nullptr);
    EXPECT_NE(resultOther, nullptr);
    EXPECT_EQ(*result, *resultOther);

    thread.join();
}
