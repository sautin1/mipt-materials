#include <iostream>
#include "ahoCorasickTrieMap.h"
#include "ahoCorasickTrieArray.h"

int main()
{
	AhoCorasickTrieArray trie;
	trie.addPattern("acab");
	trie.addPattern("accc");
	trie.addPattern("acac");
	trie.addPattern("baca");
	trie.addPattern("abb");
	trie.addPattern("z");
	trie.addPattern("ac");
	std::vector<AhoCorasickTrieArray::Match> matches;
	trie.getMatches("zabacababb", matches);
	return 0;
}

