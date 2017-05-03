#pragma once
#include <atomic>
#include <functional>
#include <memory>
#include <mutex>
#include <thread>
#include <queue>
#include <vector>

using CProcedure = std::function<void()>;

class CPackedTask {
public:
    CPackedTask(const CProcedure& _procedure, std::shared_ptr<std::atomic<bool>> _shouldStart)
        : procedure(_procedure), shouldStart(_shouldStart) {}
    bool ShouldStart() const;
    void Start() const;

private:
    CProcedure procedure;
    std::shared_ptr<std::atomic<bool>> shouldStart;
};

class CThreadPool {
public:
    CThreadPool(int threadCount = 4);
    ~CThreadPool();

    void AddTask(const CPackedTask& task);
    int ReadyThreadCount() const;

private:
    void processTasks();
    void processDeferred();

    std::atomic<int> readyThreadCount;
    std::shared_ptr<std::queue<CPackedTask>> taskQueue;
    std::shared_ptr<std::vector<CPackedTask>> deferredTasks;
    std::shared_ptr<std::atomic<bool>> shouldFinish;
    std::shared_ptr<std::mutex> queueMutex;
    std::shared_ptr<std::mutex> deferredMutex;
    std::vector<std::thread> threads;
    std::thread deferredMasterThread;
};
