// Author: Andrew Sautin

#pragma once

#include <chrono>
#include <string>

std::wstring AddLeadingZeros(const std::wstring& str, int totalLength);

int GetIntervalSeconds(const std::chrono::duration<int>& interval);
int GetIntervalMinutes(const std::chrono::duration<int>& interval);
int GetIntervalHours(const std::chrono::duration<int>& interval);
std::chrono::duration<int> CreateIntervalSeconds(int hours, int minutes, int seconds);
