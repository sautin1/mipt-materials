#pragma once
#include <string>
#include <vector>

std::vector<std::string> list_files_in_directory(const std::string& root,
                                                 const std::string& extension = "",
                                                 bool sort = false);
