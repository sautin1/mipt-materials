#pragma once

#include <atomic>
#include <functional>
#include <exception>
#include <memory>
#include <mutex>
#include <thread>

template <typename T>
class CFuture {
public:
    CFuture(const std::shared_ptr<std::shared_ptr<T>>& _value,
            const std::shared_ptr<std::shared_ptr<std::logic_error>>& _exception,
            const std::shared_ptr<std::mutex>& _mutex,
            const std::shared_ptr<std::atomic<bool>> _shouldStart = nullptr)
        : value(_value),
          exception(_exception),
          resultReadyMutex(_mutex),
          shouldStart(_shouldStart) {}

    std::shared_ptr<T> Get() const;
    std::shared_ptr<T> TryGet() const;
    void Wait() const;

//    template <class TReturnType, class... TArgs>
//    std::shared_ptr<const CFuture<T>> Then(std::function<TReturnType(TArgs...)> work) const;
private:
    std::shared_ptr<T> getOrFail() const;
    bool isDeferred() const;

    std::shared_ptr<std::shared_ptr<T>> value;
    std::shared_ptr<std::shared_ptr<std::logic_error>> exception;
    std::shared_ptr<std::mutex> resultReadyMutex;
    std::shared_ptr<std::atomic<bool>> shouldStart;
};

template <typename T>
class CPromise {
public:
    CPromise(const std::shared_ptr<std::atomic<bool>>& _shouldStart = nullptr)
        : value(new std::shared_ptr<T>()),
          exception(new std::shared_ptr<std::logic_error>()),
          resultReadyMutex(new std::mutex()),
          shouldStart(_shouldStart) {
        resultReadyMutex->lock();
    }

    void SetValue(const T& _value);
    void SetException(const std::logic_error& _exception);
    std::shared_ptr<CFuture<T>> GetFuture() const;
private:
    std::shared_ptr<std::shared_ptr<T>> value;
    std::shared_ptr<std::shared_ptr<std::logic_error>> exception;
    std::shared_ptr<std::mutex> resultReadyMutex;
    std::shared_ptr<std::atomic<bool>> shouldStart;
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
    if (resultReadyMutex->try_lock()) {
        resultReadyMutex->unlock();
        result = getOrFail();
    }
    return result;
}

template <typename T>
void CFuture<T>::Wait() const {
    if (isDeferred()) {
        shouldStart->store(true);
    }
    resultReadyMutex->lock();
    resultReadyMutex->unlock();
}

//template <typename T>
//std::shared_ptr<const CFuture<T>> CFuture<T>::Then(const std::shared_ptr<CFuture<T>> next) const {
//    Get();
//    next->Get();
//    return next;
//}

//template <typename T>
//template <class TReturnType, class... TArgs>
//std::shared_ptr<const CFuture<T>> CFuture<T>::Then(std::function<TReturnType(TArgs...)> work) const {

//}

template <typename T>
std::shared_ptr<T> CFuture<T>::getOrFail() const {
    if (*exception) {
        throw **exception;
    }
    return *value;
}

template <typename T>
bool CFuture<T>::isDeferred() const {
    return shouldStart != nullptr;
}

template <typename T>
void CPromise<T>::SetValue(const T& _value) {
    *value = std::make_shared<T>(_value);
    resultReadyMutex->unlock();
}

template <typename T>
void CPromise<T>::SetException(const std::logic_error& _exception) {
    *exception = std::make_shared<std::logic_error>(_exception);
    resultReadyMutex->unlock();
}

template <typename T>
std::shared_ptr<CFuture<T>> CPromise<T>::GetFuture() const {
    return std::make_shared<CFuture<T>>(value, exception, resultReadyMutex, shouldStart);
}
