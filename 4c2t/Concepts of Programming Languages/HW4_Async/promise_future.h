#pragma once

#include <atomic>
#include <functional>
#include <exception>
#include <memory>
#include <mutex>
#include <thread>

#include "async_exception.h"
#include "threadpool.h"


template <typename T>
struct CAsyncResult {
    std::shared_ptr<T> value;
    std::shared_ptr<CAsyncException> exception;
};


template <typename T>
class CFuture;


template <typename T>
class CPromise {
public:
    CPromise()
        : asyncResult(new CAsyncResult<T>()),
          resultReadyMutex(new std::mutex()) {
        resultReadyMutex->lock();
        future = std::make_shared<CFuture<T>>(asyncResult, resultReadyMutex);
    }

    void SetValue(const T& _value);
    void SetException(const std::exception& _exception);
    std::shared_ptr<CFuture<T>> GetFuture() const;

private:
    std::shared_ptr<CAsyncResult<T>> asyncResult;
    std::shared_ptr<std::mutex> resultReadyMutex;
    std::shared_ptr<CFuture<T>> future;
};

template <typename T>
void CPromise<T>::SetValue(const T& _value) {
    asyncResult->value = std::make_shared<T>(_value);
    resultReadyMutex->unlock();
}

template <typename T>
void CPromise<T>::SetException(const std::exception& _exception) {
    asyncResult->exception = std::make_shared<CAsyncException>(_exception.what());
    resultReadyMutex->unlock();
}

template <typename T>
std::shared_ptr<CFuture<T>> CPromise<T>::GetFuture() const {
    return std::shared_ptr<CFuture<T>>(future);
}


template <typename T>
class CFuture {
public:
    CFuture(const std::shared_ptr<CAsyncResult<T>>& _result,
            const std::shared_ptr<std::mutex>& _mutex)
        : asyncResult(_result),
          resultReadyMutex(_mutex) {}

    std::shared_ptr<T> Get() const;
    std::shared_ptr<T> TryGet() const;
    bool IsFinished() const;
    void Wait() const;

private:
    std::shared_ptr<T> getOrFail() const;

    std::shared_ptr<CAsyncResult<T>> asyncResult;
    std::shared_ptr<std::mutex> resultReadyMutex;
};

template <typename T>
std::shared_ptr<T> CFuture<T>::Get() const {
    Wait();
    std::shared_ptr<T> result = getOrFail();
    return result;
}

template <typename T>
std::shared_ptr<T> CFuture<T>::TryGet() const {
    std::shared_ptr<T> result = nullptr;
    if (IsFinished()) {
        result = getOrFail();
    }
    return result;
}

template <typename T>
bool CFuture<T>::IsFinished() const {
    bool isFinished = resultReadyMutex->try_lock();
    if (isFinished) {
        resultReadyMutex->unlock();
    }
    return isFinished;
}

template <typename T>
void CFuture<T>::Wait() const {
    resultReadyMutex->lock();
    resultReadyMutex->unlock();
}

template <typename T>
std::shared_ptr<T> CFuture<T>::getOrFail() const {
    if (asyncResult->exception) {
        throw *(asyncResult->exception);
    }
    return asyncResult->value;
}
