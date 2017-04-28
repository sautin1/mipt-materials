#pragma once
#include <atomic>
#include <functional>
#include <memory>
#include <thread>
#include <queue>
#include <vector>

using CProcedure = std::function<void()>;

class CPackedTask {
public:
    CPackedTask(const CProcedure& _procedure, const std::shared_ptr<std::atomic<bool>>& _shouldStart)
        : procedure(_procedure), shouldStart(_shouldStart) {}
    bool ShouldStart() const;
private:
    CProcedure procedure;
    std::shared_ptr<std::atomic<bool>> shouldStart;
};

class CThreadPool {
public:
    CThreadPool(int threadCount = 3);
    ~CThreadPool();

    void AddTask(const CPackedTask& task);
private:
    void processRoutines();
    void processDeferred();

    std::vector<std::thread> threads;
    std::thread deferredMasterThread;
    std::shared_ptr<std::queue<CPackedTask>> taskQueue;
    std::shared_ptr<std::vector<CPackedTask>> deferredTasks;
    std::shared_ptr<std::atomic<bool>> shouldFinish;
};
