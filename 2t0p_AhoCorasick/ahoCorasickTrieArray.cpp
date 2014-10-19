#include "ahoCorasickTrieArray.h"

AhoCorasickTrieArray::Match::Match(int newPatternIndex, int newTextPosition)
	:patternIndex(newPatternIndex), textPosition(newTextPosition) {}

AhoCorasickTrieArray::TrieNode::TrieNode(int newParentIndex, unsigned char newWayChar)
	:parentIndex(newParentIndex), suffLink(-1), suffTermLink(-1), isTermNode(false), wayChar(newWayChar) {
	memset(nextCharNode, 255, sizeof(nextCharNode));
	memset(autoGoNode, 255, sizeof(autoGoNode));
}

size_t AhoCorasickTrieArray::size() const {
	return nodes.size();
}

AhoCorasickTrieArray::AhoCorasickTrieArray()
	:nodes(1, TrieNode(0, '$')) {}

int AhoCorasickTrieArray::getAutoGoNode(int nodeIndex, unsigned char transChar) {
	if (nodes[nodeIndex].autoGoNode[(int)transChar] == -1) {
		if (nodes[nodeIndex].nextCharNode[(int)transChar] != -1) {
			nodes[nodeIndex].autoGoNode[(int)transChar] = nodes[nodeIndex].nextCharNode[(int)transChar];
		} else {
			if (nodeIndex == 0) {
				nodes[nodeIndex].autoGoNode[(int)transChar] = 0;
			} else {
				nodes[nodeIndex].autoGoNode[(int)transChar] = getAutoGoNode(getSuffLink(nodeIndex), transChar);
			}
		}
	}
	return nodes[nodeIndex].autoGoNode[(int)transChar];
}

int AhoCorasickTrieArray::getSuffLink(int nodeIndex) {
	if (nodes[nodeIndex].suffLink == -1) {
		if (nodeIndex == 0 || nodes[nodeIndex].parentIndex == 0) {
			nodes[nodeIndex].suffLink = 0;
		} else {
			nodes[nodeIndex].suffLink = getAutoGoNode(getSuffLink(nodes[nodeIndex].parentIndex), nodes[nodeIndex].wayChar);
		}
	}
	return nodes[nodeIndex].suffLink;
}

int AhoCorasickTrieArray::getSuffTermLink(int nodeIndex) {
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

void AhoCorasickTrieArray::addPattern(const std::string& pattern) {
	int currentNodeIndex = 0;
	for (size_t charIndex = 0; charIndex < pattern.size(); ++charIndex) {
		unsigned char currentChar = pattern[charIndex] - AhoCorasickTrieArray::ALPHABET_START;
		int nextNodeIndex = nodes[currentNodeIndex].nextCharNode[(int)currentChar];
		if (nextNodeIndex == -1) {
			nodes[currentNodeIndex].nextCharNode[(int)currentChar] = nodes.size();
			nodes.push_back(TrieNode(currentNodeIndex, currentChar));
			nextNodeIndex = nodes[currentNodeIndex].nextCharNode[(int)currentChar];
		}
		currentNodeIndex = nextNodeIndex;
	}
	nodes[currentNodeIndex].isTermNode = true;
	nodes[currentNodeIndex].patternIndex = patterns.size();
	patterns.push_back(pattern);
}

void AhoCorasickTrieArray::addPattern(const std::vector<std::string> patterns) {
	for (size_t patternIndex = 0; patternIndex < patterns.size(); ++patternIndex) {
		addPattern(patterns[patternIndex]);
	}
}

std::string AhoCorasickTrieArray::getPattern(int patternIndex) {
	return patterns[patternIndex];
}

void AhoCorasickTrieArray::searchMatches(int nodeIndex, int textIndex, std::vector<Match>& matches) {
	while (nodeIndex != 0) {
		if (nodes[nodeIndex].isTermNode) {
			int patternIndex = nodes[nodeIndex].patternIndex;
			matches.push_back(Match(patternIndex, textIndex + 1 - patterns[patternIndex].size()));
		}
		nodeIndex = getSuffTermLink(nodeIndex);
	}
}

void AhoCorasickTrieArray::getMatches(const std::string& text,  std::vector<Match>& matches) {
	int currentNodeIndex = 0;
	for (size_t charIndex = 0; charIndex < text.size(); ++charIndex) {
		unsigned char currentChar = text[charIndex] - AhoCorasickTrieArray::ALPHABET_START;
		currentNodeIndex = getAutoGoNode(currentNodeIndex, currentChar);
		searchMatches(currentNodeIndex, charIndex, matches);
	}
}
