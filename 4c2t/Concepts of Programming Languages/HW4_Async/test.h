#pragma once

#include <chrono>
#include <gtest/gtest.h>
#include <stdexcept>
#include <string>

#include "async.h"

class TestPromiseFuture : public testing::Test {
public:
    TestPromiseFuture() {}

    void SetUp() {
        promise = std::shared_ptr<CPromise<int>>(new CPromise<int>());
        future = promise->GetFuture();
    }

    void TearDown() {}

protected:
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
    std::shared_ptr<std::logic_error> error(new std::logic_error(error_message));
    auto work = [this, error](int workDuration) {
        std::this_thread::sleep_for(std::chrono::milliseconds(workDuration));
        promise->SetException(error);
    };
    std::thread thread(work, 50);
    thread.detach();

    try {
        *(future->Get());
        FAIL() << "Expected std::out_of_range";
    } catch (const std::logic_error& err) {
        EXPECT_EQ(err.what(), error_message);
    } catch (...) {
        FAIL() << "Expected std::out_of_range";
    }
}
