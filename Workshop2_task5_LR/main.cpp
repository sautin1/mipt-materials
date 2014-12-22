#include <iostream>
#include <fstream>
#include <stdexcept>
#include <string>

int main(int argc, char* argv[]) {
	if (argc < 2) {
		throw std::runtime_error("Too few arguments provided");
	}
	std::ifstream fin(argv[1]);
	if (!fin.good()) {
		throw std::runtime_error("Wrong filename");
	}
	int rule_quantity;
	/*for () {

	}*/
	return 0;
}

