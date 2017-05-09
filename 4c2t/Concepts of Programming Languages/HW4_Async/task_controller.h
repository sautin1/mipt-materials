#pragma once

#include <functional>
#include <memory>
#include <tuple>
#include <vector>

#include "promise_future.h"
#include "threadpool.h"


template <typename TResult>
class CTaskController {
public:
    CTaskController(std::shared_ptr<CThreadPool> _threadPool,
                    const std::function<TResult()>& _function)
        : threadPool(_threadPool), function(_function), promise(new CPromise<TResult>()) {
        future = promise->GetFuture();
    }

    void Start() const;
    void Delegate() const;
    bool IsCompleted() const;
    void WaitCompleted() const;

    std::shared_ptr<CFuture<TResult>> GetFuture() const;
    const std::function<TResult()>& GetFunction() const;

    CProcedure BuildProcedure() const;

    template <typename TResultNew>
    CTaskController<TResultNew> Then(
            std::function<TResultNew(TResult)> functionOther) const;

private:
    std::shared_ptr<CThreadPool> threadPool;
    std::function<TResult()> function;
    std::shared_ptr<CPromise<TResult>> promise;
    std::shared_ptr<CFuture<TResult>> future;
};

template <typename TResult>
void CTaskController<TResult>::Start() const {
    if (!IsCompleted()) {
        CProcedure procedure = BuildProcedure();
        procedure();
    }
}

template <typename TResult>
void CTaskController<TResult>::Delegate() const {
    if (!IsCompleted()) {
        CProcedure procedure = BuildProcedure();
        threadPool->AddTask(procedure);
    }
}

template <typename TResult>
bool CTaskController<TResult>::IsCompleted() const {
    return future != nullptr && future->IsFinished();
}

template <typename TResult>
void CTaskController<TResult>::WaitCompleted() const {
    future->Wait();
}

template <typename TResult>
CProcedure CTaskController<TResult>::BuildProcedure() const {
    CProcedure procedure = [this]() {
        try {
            promise->SetValue(function());
        } catch (const std::exception& error) {
            promise->SetException(error);
        }
    };
    return procedure;
}

template <typename TResult>
std::shared_ptr<CFuture<TResult>> CTaskController<TResult>::GetFuture() const {
    return future;
}

template <typename TResult>
const std::function<TResult()>& CTaskController<TResult>::GetFunction() const {
    return function;
}

template <typename TResult>
template <typename TResultNew>
CTaskController<TResultNew> CTaskController<TResult>::Then(
        std::function<TResultNew(TResult)> functionOther) const {
    std::function<TResultNew()> functionNew = [this, functionOther]() {
        Start();
        std::shared_ptr<CFuture<TResult>> thisFuture = GetFuture();
        std::shared_ptr<TResult> thisResult = thisFuture->Get();
        return functionOther(*thisResult);
    };
    return CTaskController<TResultNew>(threadPool, functionNew);
}
