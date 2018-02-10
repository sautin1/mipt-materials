#include <iostream>
#include <string>
#include <vector>
#include <sstream>

int main()
{
	std::string sentence;
	std::string words;
	std::getline(std::cin, sentence);
	std::getline(std::cin, words);
	std::stringstream stringStream(words);

	bool success = true;
	size_t currentPos = 0;
	size_t lastPos = 0;
	std::string word;
	while (stringStream >> word) {
		if (word.empty()) {
			break;
		}
		if (success) {
			currentPos = sentence.find(word, currentPos);
			success = (currentPos != std::string::npos);
			if (success) {
				for (size_t charIndex = lastPos; charIndex < currentPos; ++charIndex) {
					if (sentence[charIndex] != ' ') {
						sentence[charIndex] = '_';
					}
				}
			}
			currentPos += word.length();
			lastPos = currentPos;
			++currentPos;
		}
	}
	for (size_t charIndex = lastPos; charIndex < sentence.size(); ++charIndex) {
		if (sentence[charIndex] != ' ') {
			sentence[charIndex] = '_';
		}
	}
	if (success) {
		std::cout << sentence << "\n";
	} else {
		std::cout << "I HAVE FAILED!!!\n";
	}
	return 0;
}

