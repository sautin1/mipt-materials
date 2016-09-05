// Author: Andrew Sautin

#include "FileUtils.h"

std::wstring AppendFileToPath(const std::wstring& path, const std::wstring& file)
{
    std::wstring res(path);
    if (res.back() != '\\') {
        res += L'\\';
    }
    return res + file;
}

void GetFileList(const std::wstring& directory, std::vector<std::wstring>* fileList)
{
    std::wstring path = AppendFileToPath(directory, L"*");
    WIN32_FIND_DATA findData;
    HANDLE hFind = ::FindFirstFile(path.c_str(), &findData);
    fileList->clear();
    if (hFind == INVALID_HANDLE_VALUE) {
        return;
    }
    int fileExtraAttributes = FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_HIDDEN;
    do {
        if ((findData.dwFileAttributes & fileExtraAttributes) == 0) {
            fileList->emplace_back(findData.cFileName);
        }
    } while (::FindNextFile(hFind, &findData));
    ::FindClose(hFind);
}

bool CheckDirecoryExists(const std::wstring& path)
{
    DWORD attr = ::GetFileAttributes(path.c_str());
    return (attr != INVALID_FILE_ATTRIBUTES && (attr & FILE_ATTRIBUTE_DIRECTORY));
}

std::wstring GetRandomFile(const std::wstring& directory)
{
    std::vector<std::wstring> fileList;
    GetFileList(directory, &fileList);
    return fileList.empty() ? std::wstring() : fileList.at(std::rand() % fileList.size());
}
