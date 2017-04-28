#pragma once
#include "threadpool.h"
#include "promise_future.h"

#include <functional>

template <class TReturnType, class... TArgTypes>
CFuture async(bool isDeferred,
              CThreadPool& threadPool,
              const std::function<TReturnType(ArgTypes...)>& function,
              const ArgTypes&... args) {
    std::shared_ptr<CPromise<TReturnType>> promise(isDeferred);
    std::shared_ptr<CFuture<TReturnType>> future = promise->GetFuture();
    CProcedure procedure = [&promise, &function] {
        try {
            TReturnType result = function(args);
            promise->SetValue(result);
        } catch (const std::logic_error& error) {
            promise->SetException(error);
        }
    };
    std::shared_ptr<std::atomic<bool>> shouldStart = nullptr;
    if (isDeferred) {
        shouldStart = std::shared_ptr<std::atomic<bool>>(new std::atomic<bool>(false));
    }
    CPackedTask task(procedure, shouldStart);
    threadPool.AddTask(task);
    return future;
}
