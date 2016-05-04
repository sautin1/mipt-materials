#pragma once
#include <chrono>

class CTimer {
public:
    void Start();
    void Stop();
    long long GetDuration();
private:
    std::chrono::high_resolution_clock::time_point startTime;
    std::chrono::high_resolution_clock::time_point stopTime;
};
