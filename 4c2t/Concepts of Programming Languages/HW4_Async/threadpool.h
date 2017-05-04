#pragma once
#include <atomic>
#include <deque>
#include <functional>
#include <memory>
#include <mutex>
#include <thread>
#include <queue>
#include <vector>

#include "thread_semaphore.h"

using CProcedure = std::function<void()>;


class CThreadPool {
public:
    CThreadPool(int threadCount = 4);
    ~CThreadPool();

    void AddTask(const CProcedure& procedure);
    int CountReadyThreads();

private:
    void processTasks(int threadIdx);
    bool isThreadReady(int threadIdx);
    int getLeastBusyThread();

    std::atomic<bool> shouldFinish;     // poison pill

    std::deque<std::mutex> mutexes;    // for accessing queues safely (use deque since cannot store mutexes in vector)
    std::vector<CSemaphore> semaphores; // counter shows the size of queues
    std::vector<std::queue<CProcedure>> taskQueues;
    std::vector<std::thread> threads;
};
