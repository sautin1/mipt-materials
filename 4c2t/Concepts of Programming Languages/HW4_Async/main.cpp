#include <gtest/gtest.h>

#include "test_promise_future.h"
#include "test_thread_semaphore.h"
#include "test_threadpool.h"

int main(int argc, char** argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
