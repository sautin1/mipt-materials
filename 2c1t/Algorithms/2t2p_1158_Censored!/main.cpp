#include <iostream>

#include <vector>
#include <cstring>
#include <string>
#include <map>

class LongNumber {
private:
	std::vector<int> digits;
public:
	static const int BASE = 10;
	LongNumber(long number) {
		if (number == 0) {
			digits.push_back(0);
		} else {
			while (number > 0) {
				digits.push_back(number % BASE);
				number /= BASE;
			}
		}
	}
	LongNumber() {
		digits.push_back(0);
	}

	friend LongNumber operator+ (const LongNumber& left, const LongNumber& right);
	friend bool operator== (const LongNumber& left, const LongNumber& right);
	friend std::ostream& operator<<(std::ostream& sout, const LongNumber& longNumber);

	void operator += (const LongNumber& right) {
		*this = operator+(*this, right);
	}
};

LongNumber operator+ (const LongNumber& left, const LongNumber& right)
{
	LongNumber result(0);
	result.digits.assign(std::max(left.digits.size(), right.digits.size()), 0);
	int remainder = 0;
	for (size_t digitIndex = 0; digitIndex < result.digits.size(); ++digitIndex) {
		int leftDigit = 0;
		int rightDigit = 0;
		if (digitIndex < left.digits.size()) {
			leftDigit = left.digits[digitIndex];
		}
		if (digitIndex < right.digits.size()) {
			rightDigit = right.digits[digitIndex];
		}
		int sum = leftDigit + rightDigit + remainder;
		remainder = sum / LongNumber::BASE;
		result.digits[digitIndex] = sum % LongNumber::BASE;
	}
	if (remainder != 0) {
		result.digits.push_back(remainder);
	}
	return result;
}

bool operator== (const LongNumber& left, const LongNumber& right) {
	if (left.digits.size() != right.digits.size()) {
		return false;
	}
	bool success = true;
	for (size_t digitIndex = 0; digitIndex < left.digits.size(); ++digitIndex) {
		success = (left.digits[digitIndex] == right.digits[digitIndex]);
		if (!success) {
			break;
		}
	}
	return success;
}

std::ostream& operator<<(std::ostream& sout, const LongNumber& longNumber)
{
	for (int digitIndex = longNumber.digits.size() - 1; digitIndex >= 0; --digitIndex) {
		sout << longNumber.digits[digitIndex];
	}
	return sout;
}


class AhoCorasickTrieMap {
public:
	typedef std::map<unsigned char, int>::iterator MapIterator;
	struct Match {
		int patternIndex;
		int textPosition;
		Match(int newPatternIndex, int newTextPosition);
	};
	AhoCorasickTrieMap(int newAlphabetSize = 255);
	void addPattern(const std::string& pattern);
	void addPattern(const std::vector<std::string> patterns);
	void getMatches(const std::string& text, std::vector<Match>& matches);
	LongNumber countGoodWords(int maxWordLength, const std::string& alphabet);
	std::string getPattern(int patternIndex);
private:
	static const unsigned char ALPHABET_START = 33;

	struct TrieNode {
		int patternIndex;
		int parentIndex;
		std::map<unsigned char, int> nextCharNode;
		std::map<unsigned char, int> autoGoNode;
		int suffLink, suffTermLink;
		bool isTermNode;
		unsigned char wayChar;
		TrieNode(int newParentIndex, unsigned char newWayChar);
	};

	std::vector<TrieNode> nodes;
	std::vector<std::string> patterns;
	size_t alphabetSize;

	size_t size() const;
	void searchMatches(int nodeIndex, int textIndex, std::vector<Match>& matches);
	int getAutoGoNode(int nodeIndex, unsigned char transChar);
	int getSuffLink(int nodeIndex);
	int getSuffTermLink(int nodeIndex);
};

#include <iostream>

AhoCorasickTrieMap::Match::Match(int newPatternIndex, int newTextPosition)
	:patternIndex(newPatternIndex), textPosition(newTextPosition) {}

AhoCorasickTrieMap::TrieNode::TrieNode(int newParentIndex, unsigned char newWayChar)
	:parentIndex(newParentIndex), suffLink(-1), suffTermLink(-1), isTermNode(false), wayChar(newWayChar) {}


size_t AhoCorasickTrieMap::size() const {
	return nodes.size();
}

AhoCorasickTrieMap::AhoCorasickTrieMap(int newAlphabetSize)
	:nodes(1, TrieNode(0, '$')), alphabetSize(newAlphabetSize) {}

int AhoCorasickTrieMap::getAutoGoNode(int nodeIndex, unsigned char transChar) {
	if (nodes[nodeIndex].autoGoNode.find(transChar) == nodes[nodeIndex].autoGoNode.end()) {
		MapIterator it = nodes[nodeIndex].nextCharNode.find(transChar);
		if (it != nodes[nodeIndex].nextCharNode.end()) {
			nodes[nodeIndex].autoGoNode.insert(std::pair<char, int>(transChar, it->second));
		} else {
			if (nodeIndex == 0) {
				nodes[nodeIndex].autoGoNode.insert(std::pair<char, int>(transChar, 0));
			} else {
				nodes[nodeIndex].autoGoNode.insert(std::pair<char, int>(transChar, getAutoGoNode(getSuffLink(nodeIndex), transChar)));
			}
		}
	}
	return nodes[nodeIndex].autoGoNode.find(transChar)->second;
}

int AhoCorasickTrieMap::getSuffLink(int nodeIndex) {
	if (nodes[nodeIndex].suffLink == -1) {
		if (nodeIndex == 0 || nodes[nodeIndex].parentIndex == 0) {
			nodes[nodeIndex].suffLink = 0;
		} else {
			nodes[nodeIndex].suffLink = getAutoGoNode(getSuffLink(nodes[nodeIndex].parentIndex), nodes[nodeIndex].wayChar);
		}
	}
	return nodes[nodeIndex].suffLink;
}

int AhoCorasickTrieMap::getSuffTermLink(int nodeIndex) {
	if (nodes[nodeIndex].suffTermLink == -1) {
		int currentNodeSuffLink = getSuffLink(nodeIndex);
		if (currentNodeSuffLink == 0) {
			nodes[nodeIndex].suffTermLink = 0;
		} else {
			if (nodes[currentNodeSuffLink].isTermNode) {
				nodes[nodeIndex].suffTermLink = currentNodeSuffLink;
			} else {
				nodes[nodeIndex].suffTermLink = getSuffLink(currentNodeSuffLink);
			}
		}
	}
	return nodes[nodeIndex].suffTermLink;
}

void AhoCorasickTrieMap::addPattern(const std::string& pattern) {
	int currentNodeIndex = 0;
	for (size_t charIndex = 0; charIndex < pattern.size(); ++charIndex) {
		unsigned char currentChar = pattern[charIndex] - AhoCorasickTrieMap::ALPHABET_START;
		MapIterator it = nodes[currentNodeIndex].nextCharNode.find(currentChar);
		int nextNodeIndex;
		if (it == nodes[currentNodeIndex].nextCharNode.end()) {
			nextNodeIndex = nodes[currentNodeIndex].nextCharNode.insert(std::pair<char, int>(currentChar, nodes.size())).first->second;
			nodes.push_back(TrieNode(currentNodeIndex, currentChar));
		} else {
			nextNodeIndex = it->second;
		}
		currentNodeIndex = nextNodeIndex;
	}
	nodes[currentNodeIndex].isTermNode = true;
	nodes[currentNodeIndex].patternIndex = patterns.size();
	patterns.push_back(pattern);
}

void AhoCorasickTrieMap::addPattern(const std::vector<std::string> patterns) {
	for (size_t patternIndex = 0; patternIndex < patterns.size(); ++patternIndex) {
		addPattern(patterns[patternIndex]);
	}
}

std::string AhoCorasickTrieMap::getPattern(int patternIndex) {
	return patterns[patternIndex];
}

void AhoCorasickTrieMap::searchMatches(int nodeIndex, int textIndex, std::vector<Match>& matches) {
	while (nodeIndex != 0) {
		if (nodes[nodeIndex].isTermNode) {
			int patternIndex = nodes[nodeIndex].patternIndex;
			matches.push_back(Match(patternIndex, textIndex + 1 - patterns[patternIndex].size()));
		}
		nodeIndex = getSuffTermLink(nodeIndex);
	}
}

void AhoCorasickTrieMap::getMatches(const std::string& text,  std::vector<Match>& matches) {
	int currentNodeIndex = 0;
	for (size_t charIndex = 0; charIndex < text.size(); ++charIndex) {
		unsigned char currentChar = text[charIndex] - AhoCorasickTrieMap::ALPHABET_START;
		currentNodeIndex = getAutoGoNode(currentNodeIndex, currentChar);
		searchMatches(currentNodeIndex, charIndex, matches);
	}
}

LongNumber AhoCorasickTrieMap::countGoodWords(int maxWordLength, const std::string& alphabet) {
	std::vector< std::vector<LongNumber> > walkNumber(maxWordLength + 1, std::vector<LongNumber>(size(), LongNumber(0)));
	walkNumber[0][0] = LongNumber(1);
	for (int wordLength = 0; wordLength < maxWordLength; ++wordLength) {
		for (size_t sourceNode = 0; sourceNode < size(); ++sourceNode) {
			if (nodes[sourceNode].isTermNode || (walkNumber[wordLength][sourceNode] == LongNumber(0))) {
				continue;
			}
			for (size_t letterIndex = 0; letterIndex < alphabet.size(); ++letterIndex) {
				int destinationNode = getAutoGoNode(sourceNode, alphabet[letterIndex] - ALPHABET_START);
				walkNumber[wordLength+1][destinationNode] += walkNumber[wordLength][sourceNode];
			}
		}
	}
	LongNumber result = 0;
	for (size_t nodeIndex = 0; nodeIndex < size(); ++nodeIndex) {
		if (!nodes[nodeIndex].isTermNode) {
			result += walkNumber[maxWordLength][nodeIndex];
		}
	}
	return result;
}


int main()
{
	int alphabetSize, sentenceLength, badWordsQuantity;
	std::cin >> alphabetSize >> sentenceLength >> badWordsQuantity;
	std::string alphabet;
	std::getline(std::cin, alphabet);
	std::getline(std::cin, alphabet);
	AhoCorasickTrieMap trie(alphabetSize);
	std::vector<std::string> badWords;
	for (int badWordIndex = 0; badWordIndex < badWordsQuantity; ++badWordIndex) {
		std::string badWord;
		std::getline(std::cin, badWord);
		bool success = true;
		for (size_t badWordIndex = 0; badWordIndex < badWords.size(); ++badWordIndex) {
			success = (badWord.find(badWords[badWordIndex]) == std::string::npos);
			if (!success) {
				break;
			}
			success = (badWords[badWordIndex].find(badWord) == std::string::npos);
			if (!success) {
				std::swap(badWords[badWordIndex], badWord);
				break;
			}
		}
		if (success) {
			badWords.push_back(badWord);
		}
	}

	for (size_t badWordIndex = 0; badWordIndex < badWords.size(); ++badWordIndex) {
		trie.addPattern(badWords[badWordIndex]);
	}
	LongNumber result = trie.countGoodWords(sentenceLength, alphabet);
	std::cout << result << "\n";
	return 0;
}

