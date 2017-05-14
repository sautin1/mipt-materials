#pragma once

#include <functional>

#include "task_controller.h"
#include "threadpool.h"


enum class TTaskLaunchMode : char {
    SYNC,       // synchronously
    ASYNC,      // asynchronously, start at once
    DEFERRED,   // asynchronously, start deferred
    CHOOSE      // let function decide between ASYNC and SYNC
};


template <class TReturnType, class... TArgTypes>
std::shared_ptr<CTaskController> Async(std::shared_ptr<CThreadPool> threadPool,
           std::function<TReturnType(TArgTypes...)> function,
           TArgTypes... args,
           TTaskLaunchMode mode) {
    std::function<TReturnType()> functionNoArgs = std::bind(function, args...);
    std::shared_ptr<CTaskController<TReturnType>> task(
        new CTaskController<TReturnType>(threadPool, functionNoArgs)
    );

    bool isThreadPoolBusy = threadPool->CountReadyThreads() == 0;

    if (mode == TTaskLaunchMode::SYNC || (mode == TTaskLaunchMode::CHOOSE && isThreadPoolBusy)) {
        task->Start();
    } else if (mode == TTaskLaunchMode::ASYNC || mode == TTaskLaunchMode::CHOOSE) {
        task->Delegate();
    }

    return task;
}
