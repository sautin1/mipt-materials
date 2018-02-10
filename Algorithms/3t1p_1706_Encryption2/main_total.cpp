#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <set>

class SuffixArray
{
public:
	enum ArrayType {
		SUFFIX_ARRAY, SHIFT_ARRAY
	};
private:
	typedef std::unordered_map<char, size_t> CharCompressionMap;
	static const char NONEXISTING_CHAR = 1;
	std::string sourceString;
	std::vector<size_t> suffixArray;
	std::vector<int> lcp;
	ArrayType arrayType;

	void compressChars(const std::string& string, CharCompressionMap& charMap) const;
	void createSuffixArray();
	void countLCP();
public:
	SuffixArray(const std::string& string, ArrayType type);
	void printSuffixArray(std::ostream& fout, const std::string& delimiter) const;
	void printLCP(std::ostream& fout, const std::string& delimiter) const;
	size_t substringQuantity() const;
};

SuffixArray::SuffixArray(const std::string& string, ArrayType type) {
	sourceString = string;
	arrayType = type;
	if (arrayType == SUFFIX_ARRAY) {
		sourceString.push_back(NONEXISTING_CHAR);
	}
	suffixArray.assign(sourceString.size(), 0);
	lcp.assign(sourceString.size(), 0);
	createSuffixArray();
	countLCP();
}

void SuffixArray::compressChars(const std::string& string, CharCompressionMap& charMap) const {
	std::set<char> charSet;
	for (size_t charIndex = 0; charIndex < string.size(); ++charIndex) {
		charSet.insert(string[charIndex]);
	}
	size_t charCounter = 0;
	while (!charSet.empty()) {
		std::set<char>::iterator it = charSet.begin();
		char minChar = *(it);
		charSet.erase(it);
		charMap.insert(std::pair<char, size_t>(minChar, charCounter++));
	}
}

void SuffixArray::createSuffixArray() {
	CharCompressionMap charCompressionMap;
	compressChars(sourceString, charCompressionMap);

	//  Initialize suffixArray for 1-long prefix of every suffix of the string
	std::vector<size_t> classCounter(sourceString.size(), 0);
	size_t classQuantity = charCompressionMap.size();
	for (size_t charIndex = 0; charIndex < sourceString.size(); ++charIndex) {
		size_t compressedChar = charCompressionMap[sourceString[charIndex]];
		++classCounter[compressedChar];
	}
	for (size_t classIndex = 1; classIndex < classQuantity; ++classIndex) {
		classCounter[classIndex] += classCounter[classIndex - 1];
	}
	for (int suffixIndex = sourceString.size() - 1; suffixIndex >= 0; --suffixIndex) {
		size_t compressedChar = charCompressionMap[sourceString[suffixIndex]];
		size_t suffixPosition = --classCounter[compressedChar];
		suffixArray[suffixPosition] = suffixIndex;
	}
	// Initialize suffixClass for 1-long prefix of every suffix of the string
	classCounter.assign(sourceString.size(), 0);
	std::vector<size_t> shiftClass(sourceString.size());
	classQuantity = 1;
	shiftClass[suffixArray[0]] = 0;
	++classCounter[shiftClass[suffixArray[0]]];
	for (size_t suffixIndex = 1; suffixIndex < sourceString.size(); ++suffixIndex) {
		char previousChar = sourceString[suffixArray[suffixIndex - 1]];
		char  currentChar = sourceString[suffixArray[suffixIndex	]];
		if (previousChar != currentChar) {
			++classQuantity;
		}
		size_t currentClass = classQuantity - 1;
		shiftClass[suffixArray[suffixIndex]] = currentClass;
		++classCounter[currentClass];
	}

	// Count suffixArray for n-long circular shift of the string
	for (size_t shiftSize = 1; shiftSize < sourceString.size() && classQuantity < sourceString.size(); shiftSize *= 2) {
		// update suffixArray for shiftSize-long circular shift of the string
		std::vector<size_t> newSuffixArray(sourceString.size());
		for (size_t classIndex = 1; classIndex < classQuantity; ++classIndex) {
			classCounter[classIndex] += classCounter[classIndex - 1];
		}
		for (int shiftIndex = sourceString.size() - 1; shiftIndex >= 0; --shiftIndex) {
			int shiftStartPos = (suffixArray[shiftIndex] - shiftSize);
			if (shiftStartPos < 0) {
				shiftStartPos += sourceString.size();
			}
			size_t shiftOrderPos = --classCounter[shiftClass[shiftStartPos]];
			newSuffixArray[shiftOrderPos] = shiftStartPos;
		}
		suffixArray = newSuffixArray;

		// update suffixClass for shiftSize-long circular shift of the string
		classCounter.assign(sourceString.size(), 0);
		std::vector<size_t> newShiftClass(sourceString.size());
		classQuantity = 1;
		newShiftClass[suffixArray[0]] = 0;
		++classCounter[shiftClass[suffixArray[0]]];
		for (size_t shiftIndex = 1; shiftIndex < sourceString.size(); ++shiftIndex) {
			size_t previousShiftMiddle = (suffixArray[shiftIndex - 1] + shiftSize) % sourceString.size();
			size_t  currentShiftMiddle = (suffixArray[shiftIndex	] + shiftSize) % sourceString.size();
			bool needNewClass = shiftClass[ suffixArray[shiftIndex - 1] ] != shiftClass[ suffixArray[shiftIndex] ];
			needNewClass = needNewClass || shiftClass[ previousShiftMiddle ] != shiftClass[ currentShiftMiddle ];
			if (needNewClass) {
				++classQuantity;
			}
			size_t currentClass = classQuantity - 1;
			newShiftClass[suffixArray[shiftIndex]] = currentClass;
			++classCounter[currentClass];
		}
		shiftClass = newShiftClass;
	}
}

void SuffixArray::countLCP() {
	std::vector<size_t> suffixPositionInArray(sourceString.size());
	for (size_t suffixIndex = 0; suffixIndex < sourceString.size(); ++suffixIndex) {
		suffixPositionInArray[suffixArray[suffixIndex]] = suffixIndex;
	}
	size_t currentLCP = 0;
	for (size_t suffixIndex = 0; suffixIndex < sourceString.size(); ++suffixIndex) {
		if (currentLCP > 0) {
			--currentLCP;
		}
		if (suffixPositionInArray[suffixIndex] == sourceString.size() - 1) {
			lcp[sourceString.size() - 1] = -1;
			currentLCP = 0;
		} else {
			size_t nextSuffixIndex = suffixArray[suffixPositionInArray[suffixIndex] + 1];
			while (std::max(suffixIndex + currentLCP, nextSuffixIndex + currentLCP) < sourceString.size() &&
				   sourceString[suffixIndex + currentLCP] == sourceString[nextSuffixIndex + currentLCP]) {
				++currentLCP;
			}
			lcp[suffixPositionInArray[suffixIndex]] = currentLCP;
		}
	}
}

void SuffixArray::printSuffixArray(std::ostream& fout, const std::string& delimiter) const {
	size_t startIndex = 0;
	if (arrayType == SUFFIX_ARRAY) {
		++startIndex;
	}
	for (size_t suffixIndex = startIndex; suffixIndex < suffixArray.size(); ++suffixIndex) {
		fout << suffixArray[suffixIndex] << delimiter;
	}
}

void SuffixArray::printLCP(std::ostream& fout, const std::string& delimiter) const {
	size_t startIndex = 0;
	if (arrayType == SUFFIX_ARRAY) {
		++startIndex;
	}
	for (size_t suffixPairIndex = startIndex; suffixPairIndex < lcp.size(); ++suffixPairIndex) {
		fout << lcp[suffixPairIndex] << delimiter;
	}
}

size_t SuffixArray::substringQuantity() const {
	int result = 0;
	size_t startIndex = 0;
	if (arrayType == SUFFIX_ARRAY) {
		++startIndex;
	}
	for (size_t suffixIndex = startIndex; suffixIndex < sourceString.size(); ++suffixIndex) {
		result += (sourceString.size() - startIndex - suffixArray[suffixIndex]);
		if (suffixIndex > startIndex) {
			result -= lcp[suffixIndex - 1];
		}
	}
	return result;
}


int main()
{
	size_t shiftSize;
	std::cin >> shiftSize;
	std::string sourceString;
	std::cin >> sourceString;
	std::string string;
	for (size_t startIndex = 0; startIndex < sourceString.size(); ++startIndex) {
		if (startIndex == 0) {
			string = sourceString.substr(0, shiftSize);
		} else {
			string.push_back(sourceString[(startIndex + shiftSize - 1) % sourceString.size()]);
			string.erase(string.begin());
		}
		SuffixArray suffixArray(string, SuffixArray::SUFFIX_ARRAY);
		std::cout << suffixArray.substringQuantity() << " ";
	}
	return 0;
}
