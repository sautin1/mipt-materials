#ifndef AHOCORASICKTRIEMAP_H
#define AHOCORASICKTRIEMAP_H

#include <vector>
#include <cstring>
#include <string>
#include <map>

class AhoCorasickTrieMap {
public:
	typedef std::map<char, int>::iterator MapIterator;
	struct Match {
		int patternIndex;
		int textPosition;
		Match(int newPatternIndex, int newTextPosition);
	};
	AhoCorasickTrieMap();
	void addPattern(const std::string& pattern);
	void addPattern(const std::vector<std::string> patterns);
	void getMatches(const std::string& text, std::vector<Match>& matches);
	std::string getPattern(int patternIndex);
private:
	static const char ALPHABET_START = 'a';

	struct TrieNode {
		int patternIndex;
		int parentIndex;
		std::map<char, int> nextCharNode;
		std::map<char, int> autoGoNode;
		int suffLink, suffTermLink;
		bool isTermNode;
		char wayChar;
		TrieNode(int newParentIndex, char newWayChar);
	};

	std::vector<TrieNode> nodes;
	std::vector<std::string> patterns;

	size_t size() const;
	void searchMatches(int nodeIndex, int textIndex, std::vector<Match>& matches);
	int getAutoGoNode(int nodeIndex, char transChar);
	int getSuffLink(int nodeIndex);
	int getSuffTermLink(int nodeIndex);
};

#endif // AHOCORASICKTRIEMAP_H
