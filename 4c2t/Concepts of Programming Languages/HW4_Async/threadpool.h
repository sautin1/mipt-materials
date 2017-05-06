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


enum class TThreadState : char {
    BUSY, FREE
};


class CThreadPool {
public:
    CThreadPool(int _threadCount = 3);
    ~CThreadPool();

    void AddTask(const CProcedure& procedure);
    int CountReadyThreads();
    int GetThreadCount() const;

private:
    void processTasks(int threadIdx);
    bool isThreadReady(int threadIdx);
    int getLeastBusyThread();

    int threadCount;

    std::atomic<bool> shouldFinish;                     // poison pill

    std::deque<std::mutex> mutexes;                     // for accessing queues safely
                                                        // (use deque since mutexes are not moveable)
    std::vector<CSemaphore> semaphores;                 // counter shows the size of queues
    std::vector<std::queue<CProcedure>> taskQueues;
    std::vector<std::thread> threads;
    std::deque<std::atomic<TThreadState>> threadStates; // (use deque since atomics are not moveable)
};
