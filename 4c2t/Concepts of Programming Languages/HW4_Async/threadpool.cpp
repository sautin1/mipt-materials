#include "threadpool.h"

CThreadPool::CThreadPool(int _threadCount)
    : threadCount(_threadCount),
      shouldFinish(false),
      semaphores(_threadCount, CSemaphore(0)),
      taskQueues(_threadCount, std::queue<CProcedure>()) {
    threads.reserve(threadCount);
    for (int i = 0; i < threadCount; ++i) {
        threads.push_back(std::thread(&CThreadPool::processTasks, this, i));
        threadStates.emplace_back(TThreadState::FREE);
        mutexes.emplace_back();
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
    int threadIdx = getLeastBusyThread();
    mutexes[threadIdx].lock();
    taskQueues[threadIdx].push(procedure);
    semaphores[threadIdx].Post();
    mutexes[threadIdx].unlock();
}

int CThreadPool::CountReadyThreads() {
    int count = 0;
    for (unsigned int i = 0; i < threads.size(); ++i) {
        if (isThreadReady(i)) {
            ++count;
        }
    }
    return count;
}

int CThreadPool::GetThreadCount() const {
    return threadCount;
}

void CThreadPool::processTasks(int threadIdx) {
    while (true) {
        threadStates[threadIdx] = TThreadState::FREE;
        semaphores[threadIdx].Wait();
        if (shouldFinish) {
            break;
        }
        threadStates[threadIdx] = TThreadState::BUSY;
        mutexes[threadIdx].lock();
        CProcedure task = taskQueues[threadIdx].front();
        taskQueues[threadIdx].pop();
        mutexes[threadIdx].unlock();

        task();
    }
}

bool CThreadPool::isThreadReady(int threadIdx) {
    return threadStates[threadIdx] == TThreadState::FREE;
}

int CThreadPool::getLeastBusyThread() {
    int minTaskThread;
    int minTaskCount;
    for (unsigned int i = 0; i < semaphores.size(); ++i) {
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
