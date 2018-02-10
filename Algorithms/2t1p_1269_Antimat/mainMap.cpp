#include <iostream>
#include <vector>
#include <cstring>
#include <string>
#include <map>

class AhoCorasickTrieMap {
public:
	typedef std::map<char, int>::iterator MapIterator;
	
	AhoCorasickTrieMap();
	void addPattern(const std::string& pattern);
	int getMatches(const std::string& text);
private:
	struct TrieNode {
		int patternLength;
		int parentIndex;
		std::map<char, int> nextCharNode;
		std::map<char, int> autoGoNode;
		int suffLink, suffTermLink;
		bool isTermNode;
		unsigned char wayChar;
		TrieNode(int newParentIndex, unsigned char newWayChar);
	};

	std::vector<TrieNode> nodes;

	int searchMatches(int nodeIndex, int textIndex);
	int getAutoGoNode(int nodeIndex, unsigned char transChar);
	int getSuffLink(int nodeIndex);
	int getSuffTermLink(int nodeIndex);
};

AhoCorasickTrieMap::TrieNode::TrieNode(int newParentIndex, unsigned char newWayChar)
	:parentIndex(newParentIndex), suffLink(-1), suffTermLink(-1), isTermNode(false), wayChar(newWayChar) {
}

AhoCorasickTrieMap::AhoCorasickTrieMap()
	:nodes(1, TrieNode(0, 0)) {}

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
		unsigned char currentChar = pattern[charIndex];
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
	nodes[currentNodeIndex].patternLength = pattern.size();
}

int AhoCorasickTrieMap::searchMatches(int nodeIndex, int textIndex) {
	int min = textIndex + 1;
	while (nodeIndex != 0) {
		if (nodes[nodeIndex].isTermNode) {
			min = textIndex + 1 - nodes[nodeIndex].patternLength;
			break;
		} else {
			nodeIndex = getSuffTermLink(nodeIndex);
		}
	}
	return min;
}

int AhoCorasickTrieMap::getMatches(const std::string& text) {
	int currentNodeIndex = 0;
	int min = text.size();
	for (size_t charIndex = 0; charIndex < text.size(); ++charIndex) {
		currentNodeIndex = getAutoGoNode(currentNodeIndex, text[charIndex]);
		int foundMinMatch = searchMatches(currentNodeIndex, charIndex);
		if (foundMinMatch < charIndex + 1 && foundMinMatch < min) {
			min = foundMinMatch;
		}
	}
	return min;
}

int main()
{
	AhoCorasickTrieMap trie;
	size_t patternQuantity;
	std::cin >> patternQuantity;
	std::string emptyLine;
	std::getline(std::cin, emptyLine);
	for (size_t patternIndex = 0; patternIndex < patternQuantity; ++patternIndex) {
		std::string pattern;
		std::getline(std::cin, pattern);
		trie.addPattern(pattern);
	}
	size_t textLineQuantity;
	std::cin >> textLineQuantity;
	bool passed = true;
	std::getline(std::cin, emptyLine);
	int matchLineIndex;
	int matchMinPos;
	for (size_t lineIndex = 0; lineIndex < textLineQuantity; ++lineIndex) {
		std::string textLine;
		std::getline(std::cin, textLine);
		if (passed) {
			int minMatch = trie.getMatches(textLine);
			passed = (minMatch == textLine.size());
			if (!passed) {
				matchLineIndex = lineIndex + 1;
				matchMinPos = minMatch + 1;
			}
		}
	}
	if (passed) {
		std::cout << "Passed\n";
	} else {
		std::cout << matchLineIndex << " " << matchMinPos << "\n";
	}
	return 0;
}
