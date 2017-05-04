#pragma once
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
    int CountReadyThreads() const;

private:
    void processTasks(int threadId);
    bool isThreadReady(int threadId) const;
    int getLeastBusyThread() const;

    std::atomic<bool> shouldFinish;     // poison pill

    std::vector<std::mutex> mutexes;    // for accessing queues safely
    std::vector<CSemaphore> semaphores; // counter shows the size of queues
    std::vector<std::queue<CProcedure>> taskQueues;
    std::vector<std::thread> threads;
};
