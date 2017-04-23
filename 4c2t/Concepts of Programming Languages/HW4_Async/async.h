#pragma once

#include <exception>
#include <memory>
#include <mutex>
#include <thread>

template<typename T>
class CFuture {
public:
    CFuture(const std::shared_ptr<std::shared_ptr<T>>& _value,
            const std::shared_ptr<std::shared_ptr<std::logic_error>>& _exception,
            const std::shared_ptr<std::mutex> _mutex)
        : value(_value),
          exception(_exception),
          mutex(_mutex) {}

    std::shared_ptr<T> Get() const;
    std::shared_ptr<T> TryGet() const;
private:
    std::shared_ptr<T> getOrFail() const;

    std::shared_ptr<std::shared_ptr<T>> value;
    std::shared_ptr<std::shared_ptr<std::logic_error>> exception;
    std::shared_ptr<std::mutex> mutex;
};

template<typename T>
class CPromise {
public:
    CPromise()
        : value(new std::shared_ptr<T>()),
          exception(new std::shared_ptr<std::logic_error>()),
          mutex(new std::mutex()) {
        mutex->lock();
    }

    void SetValue(const T& _value);
    void SetException(std::shared_ptr<std::logic_error> _exception);
    std::shared_ptr<CFuture<T>> GetFuture() const;
private:
    std::shared_ptr<std::shared_ptr<T>> value;
    std::shared_ptr<std::shared_ptr<std::logic_error>> exception;
    std::shared_ptr<std::mutex> mutex;
};

template<typename T>
std::shared_ptr<T> CFuture<T>::Get() const {
    mutex->lock();
    std::shared_ptr<T> result = getOrFail();
    mutex->unlock();
    return result;
}

template<typename T>
std::shared_ptr<T> CFuture<T>::TryGet() const {
    std::shared_ptr<T> result = nullptr;
    if (mutex->try_lock()) {
        result = getOrFail();
        mutex->unlock();
    }
    return result;
}

template<typename T>
std::shared_ptr<T> CFuture<T>::getOrFail() const {
    if (*exception) {
        throw **exception;
    }
    return *value;
}

template<typename T>
void CPromise<T>::SetValue(const T& _value) {
    *value = std::make_shared<T>(_value);
    mutex->unlock();
}

template<typename T>
void CPromise<T>::SetException(std::shared_ptr<std::logic_error> _exception) {
    *exception = _exception;
    mutex->unlock();
}

template<typename T>
std::shared_ptr<CFuture<T>> CPromise<T>::GetFuture() const {
    return std::make_shared<CFuture<T>>(value, exception, mutex);
}
