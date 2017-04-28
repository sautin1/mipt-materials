#include "threadpool.h"

bool CPackedTask::ShouldStart() const {
    return !shouldStart || shouldStart->load();
}

CThreadPool::CThreadPool(int threadCount)
    : deferredMasterThread(&CThreadPool::processDeferred, this),
      taskQueue(new std::queue<CPackedTask>()),
      deferredTasks(new std::vector<CPackedTask>()),
      shouldFinish(new std::atomic<bool>(false)) {
    threads.reserve(threadCount);
    for (int i = 0; i < threadCount; ++i) {
        threads.push_back(std::thread(&CThreadPool::processRoutines, this));
    }
}

CThreadPool::~CThreadPool() {
    shouldFinish->store(true);
    for (unsigned int i = 0; i < threads.size(); ++i) {
        threads[i].join();
    }
    deferredMasterThread.join();
}

void CThreadPool::AddTask(const CPackedTask& task) {
    if (task.ShouldStart()) {
        taskQueue->push(task);
    } else {
        deferredTasks->push_back(task);
    }
}

void CThreadPool::processRoutines() {
    // access non-blocking queue
    if (shouldFinish->load()) {
        return;
    }
}

void CThreadPool::processDeferred() {
    // add synchronization
    for (unsigned int i = 0; i < deferredTasks->size(); ++i) {
        if (deferredTasks->at(i).ShouldStart()) {
            std::swap(deferredTasks->at(i), deferredTasks->back());
            taskQueue->push(deferredTasks->back());
            deferredTasks->pop_back();
        }
    }
    if (shouldFinish->load()) {
        return;
    }
}
