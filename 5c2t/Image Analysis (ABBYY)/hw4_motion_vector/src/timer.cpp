#include "timer.h"
#include <chrono>


void Timer::start() {
    start_time = std::chrono::high_resolution_clock::now();
}

void Timer::stop() {
    stop_time = std::chrono::high_resolution_clock::now();
}

long long Timer::get_duration() {
    return std::chrono::duration_cast<std::chrono::microseconds>(stop_time - start_time).count();
}
