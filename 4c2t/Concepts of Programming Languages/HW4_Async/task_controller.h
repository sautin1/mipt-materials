#pragma once

#include <functional>
#include <memory>
#include <tuple>
#include <vector>

#include "promise_future.h"
#include "threadpool.h"


template <typename TResultType>
class ITaskController {
public:
    virtual void Start() = 0;
    virtual void Delegate() = 0;
    virtual bool IsCompleted() = 0;
    virtual void WaitCompleted() = 0;
};


template <typename TResultType>
class CTaskController : public ITaskController<TResultType> {
public:
    CTaskController(std::shared_ptr<CThreadPool> _threadPool,
                    const std::function<TResultType()>& _function)
        : threadPool(_threadPool), function(_function) {}

    virtual void Start() override;
    virtual void Delegate() override;
    virtual bool IsCompleted() override;
    virtual void WaitCompleted() override;

    std::shared_ptr<CFuture<TResultType>> GetFuture() const;
    const std::function<TResultType()>& GetFunction() const;

    CProcedure BuildProcedure();

private:
    std::shared_ptr<CThreadPool> threadPool;
    std::function<TResultType()> function;
    std::shared_ptr<CFuture<TResultType>> future;
};

template <typename TResultType>
void CTaskController<TResultType>::Start() {
    if (!IsCompleted()) {
        CProcedure procedure = BuildProcedure();
        procedure();
    }
}

template <typename TResultType>
void CTaskController<TResultType>::Delegate() {
    if (!IsCompleted()) {
        CProcedure procedure = BuildProcedure();
        threadPool->AddTask(procedure);
    }
}

template <typename TResultType>
bool CTaskController<TResultType>::IsCompleted() {
    return future != nullptr && future->IsFinished();
}

template <typename TResultType>
void CTaskController<TResultType>::WaitCompleted() {
    future->Wait();
}

template <typename TResultType>
CProcedure CTaskController<TResultType>::BuildProcedure() {
    std::shared_ptr<CPromise<TResultType>> promise(new CPromise<TResultType>());
    future = promise->GetFuture();
    CProcedure procedure = [this, promise]() {
        try {
            promise->SetValue(function());
        } catch (const std::exception& error) {
            promise->SetException(error);
        }
    };
    return procedure;
}

template <typename TResultType>
std::shared_ptr<CFuture<TResultType>> CTaskController<TResultType>::GetFuture() const {
    return future;
}

template <typename TResultType>
const std::function<TResultType()>& CTaskController<TResultType>::GetFunction() const {
    return function;
}


//template <typename TResultType>
//class CTaskChainController : public ITaskController<TResultType> {
//public:
//    virtual void Start() override;
//    virtual void Delegate() override;
//    virtual std::shared_ptr<CFuture<TResultType>> GetFuture() override;
//    virtual bool IsCompleted() override;
//    virtual void WaitCompleted() override;

//private:

//};
