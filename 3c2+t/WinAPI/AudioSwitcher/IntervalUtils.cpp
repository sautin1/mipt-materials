// Author: Andrew Sautin

#include "IntervalUtils.h"

std::wstring AddLeadingZeros(const std::wstring& str, int totalLength)
{
    return std::wstring(totalLength - str.length(), '0') + str;
}

int GetIntervalSeconds(const std::chrono::duration<int>& interval)
{
    return interval.count() - 60 * (GetIntervalHours(interval) * 60 + GetIntervalMinutes(interval));
}

int GetIntervalMinutes(const std::chrono::duration<int>& interval)
{
    return std::chrono::duration_cast<std::chrono::minutes>(interval).count() - GetIntervalHours(interval) * 60;
}

int GetIntervalHours(const std::chrono::duration<int>& interval)
{
    return std::chrono::duration_cast<std::chrono::hours>(interval).count();
}

std::chrono::duration<int> CreateIntervalSeconds(int hours, int minutes, int seconds)
{
    return std::chrono::duration<int>(seconds + 60 * (minutes + 60 * hours));
}
