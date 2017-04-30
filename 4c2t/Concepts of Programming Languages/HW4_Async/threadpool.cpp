#include "threadpool.h"

bool CPackedTask::ShouldStart() const {
    return !shouldStart || shouldStart->load();
}

void CPackedTask::Start() const {
    procedure();
}

CThreadPool::CThreadPool(int threadCount)
    : deferredMasterThread(&CThreadPool::processDeferred, this),
      taskQueue(new std::queue<CPackedTask>()),
      deferredTasks(new std::vector<CPackedTask>()),
      shouldFinish(new std::atomic<bool>(false)),
      queueMutex(new std::mutex()),
      deferredMutex(new std::mutex()) {
    threads.reserve(threadCount);
    for (int i = 0; i < threadCount; ++i) {
        threads.push_back(std::thread(&CThreadPool::processTasks, this));
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

void CThreadPool::processTasks() {
    while (!shouldFinish->load()) {
        queueMutex->lock();
        if (!taskQueue->empty()) {
            CPackedTask task = taskQueue->front();
            taskQueue->pop();
            queueMutex->unlock();
            try {
                task.Start();
            } catch (const std::exception& error) {
                shouldFinish->store(true);
                throw error;
            }
        } else {
            queueMutex->unlock();
        }
    }
}

void CThreadPool::processDeferred() {
    while (!shouldFinish) {
        std::unique_lock<std::mutex> lock(*deferredMutex);
        for (unsigned int i = 0; i < deferredTasks->size(); ++i) {
            if (deferredTasks->at(i).ShouldStart()) {
                std::swap(deferredTasks->at(i), deferredTasks->back());
                taskQueue->push(deferredTasks->back());
                deferredTasks->pop_back();
            }
        }
    }
}
