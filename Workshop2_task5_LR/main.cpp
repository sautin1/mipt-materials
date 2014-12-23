#include <iostream>
#include <fstream>
#include <stdexcept>
#include <string>

#include "grammar.h"

int main() {
	std::string filename;
	std::cin >> filename;
	std::ifstream fin(filename);
	if (!fin.good()) {
		throw std::runtime_error("Wrong filename");
	}
	Grammar grammar = readGrammar(fin);

	return 0;
}

