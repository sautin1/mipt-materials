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
};
