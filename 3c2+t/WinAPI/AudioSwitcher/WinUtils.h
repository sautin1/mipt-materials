// Author: Andrew Sautin

#pragma once
#include <sstream>
#include <string>
#include <Windows.h>

//Returns the last Win32 error, in wstring format. Returns an empty wstring if there is no error.
std::wstring GetLastErrorAsWString();

int OpenFileWithDefaultProgram(const std::wstring& path, int showMode, HANDLE* hProc);

template <typename T>
std::wstring ConvertToWString(const T& arg);

template <typename T>
T ConvertFromWString(const std::wstring& arg);

template <typename T>
std::wstring ConvertToWString(const T& arg) {
    std::wstringstream sstream;
    sstream << arg;
    return std::wstring(sstream.str());
}

template <typename T>
T ConvertFromWString(const std::wstring& arg) {
    std::wstringstream sstream(arg);
    T res;
    sstream >> res;
    return res;
}
