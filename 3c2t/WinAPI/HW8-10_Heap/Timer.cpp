#include "Timer.h"

void CTimer::Start()
{
    startTime = std::chrono::high_resolution_clock::now();
}

void CTimer::Stop()
{
    stopTime = std::chrono::high_resolution_clock::now();
}

long long CTimer::GetDuration()
{
    return std::chrono::duration_cast<std::chrono::microseconds>(stopTime - startTime).count();
}
