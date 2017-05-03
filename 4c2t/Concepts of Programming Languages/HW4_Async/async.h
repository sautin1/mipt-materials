#pragma once
#include "task_mode.h"
#include "threadpool.h"

#include <functional>

template <typename T>
class CFuture;

template <typename T>
class CPromise;

template <class TReturnType, class... TArgTypes>
std::shared_ptr<CFuture<TReturnType>> Async(
        CThreadPool& threadPool,
        const std::function<TReturnType(TArgTypes...)>& function,
        const TArgTypes&... args,
        TTaskLaunchMode mode = TTaskLaunchMode::ASYNC
        ) {
    std::shared_ptr<std::atomic<bool>> shouldStart(new std::atomic<bool>(mode != TTaskLaunchMode::DEFERRED));
    std::shared_ptr<CPromise<TReturnType>> promise(std::make_shared<CPromise<TReturnType>>(shouldStart));
    std::shared_ptr<CFuture<TReturnType>> future = promise->GetFuture();
    // capture by value since they are passed to another thread
    CProcedure procedure = [promise, function, args...] {
        try {
            TReturnType result = function(args...);
            promise->SetValue(result);
        } catch (const std::logic_error& error) {
            promise->SetException(error);
        }
    };
    CPackedTask task(procedure, shouldStart);
    if (mode == TTaskLaunchMode::SYNC || (mode == TTaskLaunchMode::CHOOSE && threadPool.ReadyThreadCount() == 0)) {
        // run synchronously
        task.Start();
    } else {
        // run asynchronously
        threadPool.AddTask(task);
    }
    return future;
}
