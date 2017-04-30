#include <gtest/gtest.h>

#include "promise_future.h"

//class TestFutureChain : public testing::Test {
//public:
//    TestFutureChain() : size(3) {}

//    void SetUp() {
//        for (int i = 0; i < size; ++i) {
//            promises.emplace_back(new CPromise<int>());
//            futures.push_back(promises[i]->GetFuture());
//        }
//    }

//    void TearDown() {}

//protected:
//    const int size;
//    std::vector<std::shared_ptr<CPromise<int>>> promises;
//    std::vector<std::shared_ptr<CFuture<int>>> futures;
//};

//TEST_F(TestFutureChain, DISABLED_WithoutExceptions) {
//    auto work = [this](int workDuration, int index) {
//        std::this_thread::sleep_for(std::chrono::milliseconds(workDuration));
//        promises[index]->SetValue(index);
//    };
//    for (size_t i = 0; i < promises.size(); ++i) {
//        std::thread thread(work, 50, i);
//        thread.detach();
//    }

////    futures[0]->Then(futures[1])->Then(futures[2]);
//    for (size_t i = 0; i < futures.size(); ++i) {
//        std::shared_ptr<int> result = futures[i]->TryGet();
//        EXPECT_NE(result, nullptr);
//        EXPECT_EQ(*result, i);
//    }
//}

//TEST_F(TestFutureChain, DISABLED_Exception) {
//    auto work = [this](int workDuration, int index) {
//        std::this_thread::sleep_for(std::chrono::milliseconds(workDuration));
//        promises[index]->SetValue(index);
//    };
//    std::string error_message = "It's a trap!";
//    std::shared_ptr<std::logic_error> error(new std::logic_error(error_message));
//    auto fail = [this, error](int workDuration, int index) {
//        std::this_thread::sleep_for(std::chrono::milliseconds(workDuration));
//        promises[index]->SetException(error);
//    };
//    std::vector<std::thread> threads;
//    threads.emplace_back(work, 50, 0);
//    threads.emplace_back(fail, 50, 1);
//    threads.emplace_back(work, 50000, 2); // must be skipped due to an exception in second thread

//    for (size_t i = 0; i < threads.size(); ++i) {
//        threads[i].detach();
//    }

//    try {
////        futures[0]->Then(futures[1])->Then(futures[2]);
//        FAIL() << "Expected std::logic_error";
//    } catch (const std::logic_error& err) {
//        EXPECT_EQ(err.what(), error_message);
//    } catch (...) {
//        FAIL() << "Expected std::logic_error";
//    }

//    std::shared_ptr<int> result = futures[0]->TryGet();
//    EXPECT_NE(result, nullptr);
//    EXPECT_EQ(*result, 0);

//    try {
//        futures[1]->TryGet();
//        FAIL() << "Expected std::logic_error";
//    } catch (const std::logic_error& err) {
//        EXPECT_EQ(err.what(), error_message);
//    } catch (...) {
//        FAIL() << "Expected std::logic_error";
//    }

//    result = futures[2]->TryGet();
//    EXPECT_EQ(result, nullptr);
//}
