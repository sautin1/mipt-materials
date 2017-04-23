#pragma once

#include <exception>
#include <memory>

template<typename T>
class CFuture {
public:
    CFuture(const std::shared_ptr<std::shared_ptr<T>>& _value,
            const std::shared_ptr<std::shared_ptr<std::exception>>& _exception)
        : value(_value),
          exception(_exception) {}

    std::shared_ptr<T> Get() const;
    std::shared_ptr<T> TryGet() const;
private:
    std::shared_ptr<std::shared_ptr<T>> value;
    std::shared_ptr<std::shared_ptr<std::exception>> exception;
};

template<typename T>
class CPromise {
public:
    CPromise()
        : value(new std::shared_ptr<T>()),
          exception(new std::shared_ptr<std::exception>()) {}

    void SetValue(const T& _value);
    void SetException(const std::exception& _exception);
    CFuture<T> GetFuture() const;
private:
    std::shared_ptr<std::shared_ptr<T>> value;
    std::shared_ptr<std::shared_ptr<std::exception>> exception;
};

template<typename T>
std::shared_ptr<T> CFuture<T>::Get() const {
    // wait
    return TryGet();
}

template<typename T>
std::shared_ptr<T> CFuture<T>::TryGet() const {
    if (*exception) {
        throw **exception;
    }
    return *value;
}

template<typename T>
void CPromise<T>::SetValue(const T& _value) {
    *value = std::make_shared<T>(_value);
}

template<typename T>
void CPromise<T>::SetException(const std::exception& _exception) {
    *exception = std::make_shared<std::exception>(_exception);
}

template<typename T>
CFuture<T> CPromise<T>::GetFuture() const {
    return CFuture<T>(value, exception);
}
