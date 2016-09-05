// Author: Andrew Sautin

#pragma once
#include <string>
#include <vector>
#include <Windows.h>

std::wstring AppendFileToPath(const std::wstring& path, const std::wstring& file);
void GetFileList(const std::wstring& directory, std::vector<std::wstring>* fileList);
bool CheckDirecoryExists(const std::wstring& path);
std::wstring GetRandomFile(const std::wstring& directory);
