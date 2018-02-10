#ifndef HASHING_H
#define HASHING_H

#include <stdexcept>
#include <string>
#include <vector>

#include "segment_tree.h"

typedef unsigned long long HashType;

const HashType MODULUS_NEAR_LONG_LONG_MAX = 18446744073709551557U;
const int DEFAULT_PRIME_BASE = 37;

class StringHash {
public:
	StringHash(const std::string& sample, int prime_base, HashType modulus);

	bool isPalindromicSubstring(int left_index, int right_index);
	void changeChar(int index, char new_char);
private:
	bool isEqualHash(HashType first_hash, int first_index, HashType second_hash, int second_index) const;
	HashType countCharHash(int index, char new_char) const;
	int getReversedIndex(int index) const;

	std::string sample_;
	//std::vector<HashType> prefix_hash_;
	std::vector<HashType> prime_powers_;
	HashType modulus_;
	SegmentTree<HashType> direct_hash_tree_;
	SegmentTree<HashType> reversed_hash_tree_;
};

#endif // HASHING_H
