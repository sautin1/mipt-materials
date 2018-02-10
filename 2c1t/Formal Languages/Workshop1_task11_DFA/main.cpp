#include <iostream>
#include <string>
#include <stdexcept>
#include "automaton.h"

int main()
{
	std::string regexp;
	std::string word;
	std::cin >> regexp;
	std::cin >> word;
	Automaton automaton;
	try {
		automaton = Automaton(regexp);
	} catch (std::logic_error exception) {
		std::cerr << "Incorrect regular expression: " << exception.what() << " \n";
		return 1;
	}

	int maxRecognizedSubstrLength = automaton.maxRecognizedWordSubstrLength(word);
	std::cout << maxRecognizedSubstrLength << "\n";

	return 0;
}

