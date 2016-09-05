// Author: Andrew Sautin

#pragma once
#include <chrono>
#include <fstream>
#include <sstream>
#include <string>
#include <Windows.h>

#include "IntervalUtils.h"

struct CSwitcherSettings {
    std::wstring path;
    std::chrono::duration<int> interval;
};

struct CSwitcherSettingsSerializer {
    static bool WriteSettingsToFile(const std::wstring& fileName, const CSwitcherSettings& settings);
    static bool ReadSettingsFromFile(const std::wstring& fileName, CSwitcherSettings* settings);
    static const int maxPathLength;
};

class CSwitcher {
public:
    CSwitcher();
    ~CSwitcher();

    void InitSwitcherSettings();
    const std::wstring& GetSettingsFileName() const;
    
    CSwitcherSettings settingsDefault;
    CSwitcherSettings settingsActive;
private:
    static const std::wstring settingsFileName;
};
