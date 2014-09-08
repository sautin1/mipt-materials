#include <iostream>
#include "planner.h"

int main(int argc, char* argv[])
{
    std::string filename;
    if (argc > 2) {
        throw std::logic_error("Error: wrong number of parameters. Call: \"hwp\" or \"hwp <filename>\".\n");
    } else if (argc == 2) {
        filename = std::string(argv[1]);
    }
    Planner planner(filename);
    planner.communicate();
    return 0;
}

