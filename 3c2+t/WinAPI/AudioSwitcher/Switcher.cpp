// Author: Andrew Sautin

#include "Switcher.h"

const std::wstring CSwitcher::settingsFileName = L"swtchr_def_configs.dat";

const int CSwitcherSettingsSerializer::maxPathLength = MAX_PATH;

bool CSwitcherSettingsSerializer::WriteSettingsToFile(const std::wstring& fileName, const CSwitcherSettings& settings) {
    bool success = false;
    std::wofstream fout(fileName);
    if (fout.good()) {
        fout << settings.path << std::endl;
        fout << settings.interval.count();
        fout.close();
        success = true;
    }
    return success;
}

bool CSwitcherSettingsSerializer::ReadSettingsFromFile(const std::wstring& fileName, CSwitcherSettings* settings) {
    bool success = false;
    std::wifstream fin(fileName);
    if (fin.good()) {
        wchar_t* path = new wchar_t[maxPathLength];
        fin.getline(path, maxPathLength);
        settings->path = std::wstring(path);
        int seconds = 0;
        fin >> seconds;
        settings->interval = std::chrono::duration<int>(seconds);
        success = true;
    }
    return success;
}

CSwitcher::CSwitcher()
{
}

CSwitcher::~CSwitcher()
{
}

void CSwitcher::InitSwitcherSettings()
{
    if (!CSwitcherSettingsSerializer::ReadSettingsFromFile(settingsFileName, &settingsDefault)) {
        settingsDefault.path = L"C:\\";
        settingsDefault.interval = std::chrono::duration_cast<std::chrono::seconds>(std::chrono::duration<int, std::ratio<60>>(15));
    }
    settingsActive = settingsDefault;
}

const std::wstring& CSwitcher::GetSettingsFileName() const
{
    return settingsFileName;
}
