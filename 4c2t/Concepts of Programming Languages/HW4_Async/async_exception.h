#pragma once
#include <stdexcept>

class CAsyncException : public std::logic_error {
public:
    explicit CAsyncException(const std::string& _message)
        : std::logic_error(_message) {}

    explicit CAsyncException(const std::exception& _exception)
        : std::logic_error(_exception.what()) {}
};
