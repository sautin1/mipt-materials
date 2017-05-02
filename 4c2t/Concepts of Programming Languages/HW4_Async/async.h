#pragma once
#include "threadpool.h"
#include "promise_future.h"

#include <functional>

template <class TReturnType, class... TArgTypes>
std::shared_ptr<CFuture<TReturnType>> Async(bool isDeferred,
              CThreadPool& threadPool,
              const std::function<TReturnType(TArgTypes...)>& function,
              const TArgTypes&... args) {
    std::shared_ptr<std::atomic<bool>> shouldStart = nullptr;
    if (isDeferred) {
        shouldStart = std::shared_ptr<std::atomic<bool>>(new std::atomic<bool>(false));
    }
    std::shared_ptr<CPromise<TReturnType>> promise(std::make_shared<CPromise<TReturnType>>(shouldStart));
    std::shared_ptr<CFuture<TReturnType>> future = promise->GetFuture();
    CProcedure procedure = [&promise, &function, &args...] {
        try {
            TReturnType result = function(args...);
            promise->SetValue(result);
        } catch (const std::logic_error& error) {
            promise->SetException(error);
        }
    };
    CPackedTask task(procedure, shouldStart);
    threadPool.AddTask(task);
    return future;
}
