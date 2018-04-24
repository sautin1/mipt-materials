#pragma once
#include <chrono>


class Timer {
public:
    void start();
    void stop();
    long long get_duration();
private:
    std::chrono::high_resolution_clock::time_point start_time;
    std::chrono::high_resolution_clock::time_point stop_time;
};
