#ifndef AHOCORASICKTRIEARRAY_H
#define AHOCORASICKTRIEARRAY_H

#include <vector>
#include <cstring>
#include <string>

class AhoCorasickTrieArray {
public:
	struct Match {
		int patternIndex;
		int textPosition;
		Match(int newPatternIndex, int newTextPosition);
	};
	AhoCorasickTrieArray();
	void addPattern(const std::string& pattern);
	void addPattern(const std::vector<std::string> patterns);
	void getMatches(const std::string& text, std::vector<Match>& matches);
	std::string getPattern(int patternIndex);
private:
	static const int ALPHABET_SIZE = 26;
	static const unsigned char ALPHABET_START = 'a';

	struct TrieNode {
		int patternIndex;
		int parentIndex;
		int nextCharNode[AhoCorasickTrieArray::ALPHABET_SIZE];
		int autoGoNode[AhoCorasickTrieArray::ALPHABET_SIZE];
		int suffLink, suffTermLink;
		bool isTermNode;
		unsigned char wayChar;
		TrieNode(int newParentIndex, unsigned char newWayChar);
	};

	std::vector<TrieNode> nodes;
	std::vector<std::string> patterns;

	size_t size() const;
	void searchMatches(int nodeIndex, int textIndex, std::vector<Match>& matches);
	int getAutoGoNode(int nodeIndex, unsigned char transChar);
	int getSuffLink(int nodeIndex);
	int getSuffTermLink(int nodeIndex);
};
#endif // AHOCORASICKTRIEARRAY_H
