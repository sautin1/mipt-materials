// Author: Andrew Sautin

#include "WinUtils.h"

std::wstring GetLastErrorAsWString()
{
    DWORD errorMessageID = ::GetLastError();
    if (errorMessageID == 0) {
        return std::wstring();
    }
    LPWSTR messageBuffer = nullptr;
    size_t size = FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL, errorMessageID, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPWSTR)&messageBuffer, 0, NULL);

    std::wstring message(messageBuffer, size);
    LocalFree(messageBuffer);

    return message;
}

int OpenFileWithDefaultProgram(const std::wstring& path, int showMode, HANDLE* hProc)
{
    //return (int)::ShellExecute(NULL, L"open", path.c_str(), NULL, NULL, showMode);
    SHELLEXECUTEINFO shInfo;
    ::ZeroMemory(&shInfo, sizeof(SHELLEXECUTEINFO));
    shInfo.cbSize = sizeof(SHELLEXECUTEINFO);
    shInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
    shInfo.hwnd = NULL;
    shInfo.lpVerb = L"open";
    shInfo.lpFile = path.c_str();
    shInfo.nShow = showMode;
    ::ShellExecuteEx(&shInfo);
    *hProc = shInfo.hProcess;
    return (int)shInfo.hInstApp;
}
