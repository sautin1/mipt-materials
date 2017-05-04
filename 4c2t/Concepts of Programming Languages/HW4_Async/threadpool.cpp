#include "threadpool.h"

CThreadPool::CThreadPool(int threadCount)
    : shouldFinish(false),
      mutexes(threadCount, std::mutex()),
      semaphores(threadCount, CSemaphore(0)),
      taskQueues(threadCount, std::queue<CProcedure>()) {
    threads.reserve(threadCount);
    for (int i = 0; i < threadCount; ++i) {
        threads.push_back(std::thread(&CThreadPool::processTasks, this, i));
    }
}

CThreadPool::~CThreadPool() {
    shouldFinish = true;
    for (unsigned int i = 0; i < semaphores.size(); ++i) {
        semaphores[i].Post();
        threads[i].join();
    }
}

void CThreadPool::AddTask(const CProcedure& procedure) {
    int threadId = getLeastBusyThread();
    mutexes[threadId].lock();
    taskQueues[threadId].push(procedure);
    mutexes[threadId].unlock();
}

int CThreadPool::CountReadyThreads() const {
    int count = 0;
    for (unsigned int i = 0; i < semaphores.size(); ++i) {
        if (isThreadReady(i)) {
            ++count;
        }
    }
    return count;
}

void CThreadPool::processTasks(int threadId) {
    while (true) {
        semaphores[threadId].Wait();
        if (shouldFinish) {
            break;
        }
        mutexes[threadId].lock();
        CProcedure task = taskQueues[threadId].front();
        taskQueues[threadId].pop();
        mutexes[threadId].unlock();

        task();
    }
}

bool CThreadPool::isThreadReady(int threadId) const {
    return semaphores[threadId].GetValue() == 0;
}

int CThreadPool::getLeastBusyThread() const {
    int minTaskThread;
    int minTaskCount;
    for (int i = 0; i < semaphores.size(); ++i) {
        int currentValue = semaphores[i].GetValue();
        if (i == 0) {
            minTaskThread = 0;
            minTaskCount = currentValue;
        } else if (currentValue < minTaskCount) {
            minTaskThread = i;
            minTaskCount = currentValue;
        }

        if (currentValue == 0) {
            break;
        }
    }
    return minTaskThread;
}
