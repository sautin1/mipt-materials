#include "os.h"
#include <algorithm>
#include <dirent.h>
#include <stdexcept>


std::vector<std::string> list_files_in_directory(const std::string& root, const std::string& extension, bool sort) {
    std::vector<std::string> paths;
    DIR* dir = opendir(root.c_str());
    if (dir == nullptr) {
        throw std::runtime_error("Cannot open directory");
    }
    for (dirent* entry = readdir(dir); entry != nullptr; entry = readdir(dir)) {
        std::string entry_name(entry->d_name);
        if (entry->d_type == DT_REG && entry_name.substr(entry_name.size() - extension.size()) == extension) {
            paths.push_back(root + '/' + entry->d_name);
        }
    }
    closedir(dir);
    if (sort) {
        std::sort(paths.begin(), paths.end());
    }
    return paths;
}
