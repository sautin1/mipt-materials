#include "ahoCorasickTrieMap.h"

AhoCorasickTrieMap::Match::Match(int newPatternIndex, int newTextPosition)
	:patternIndex(newPatternIndex), textPosition(newTextPosition) {}

AhoCorasickTrieMap::TrieNode::TrieNode(int newParentIndex, unsigned char newWayChar)
	:parentIndex(newParentIndex), suffLink(-1), suffTermLink(-1), isTermNode(false), wayChar(newWayChar) {}


size_t AhoCorasickTrieMap::size() const {
	return nodes.size();
}

AhoCorasickTrieMap::AhoCorasickTrieMap()
	:nodes(1, TrieNode(0, '$')) {}

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
