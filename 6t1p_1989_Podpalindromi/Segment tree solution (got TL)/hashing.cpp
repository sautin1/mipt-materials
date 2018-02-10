#include "hashing.h"

StringHash::StringHash(const std::string& sample, int prime_base, HashType modulus)
	: sample_(sample), prime_powers_(sample.size(), 0),
	  modulus_(modulus) {
	std::vector<HashType> prefix_hash(sample.size(), 0);
	// count prime_base powers
	prime_powers_[0] = 1;
	for (size_t prime_power = 1; prime_power < sample_.size(); ++prime_power) {
		prime_powers_[prime_power] = prime_powers_[prime_power - 1] * prime_base % modulus;
	}

	// count char hashes
	for (size_t prefix_index = 0; prefix_index < sample_.size(); ++prefix_index) {
		prefix_hash[prefix_index] = countCharHash(prefix_index, sample_[prefix_index]);
	}

	// create segment trees
	direct_hash_tree_ = SegmentTree<HashType>(prefix_hash);
	for (size_t prefix_index = 0; prefix_index < sample_.size(); ++prefix_index) {
		int reversed_index = getReversedIndex(prefix_index);
		prefix_hash[prefix_index] = countCharHash(prefix_index, sample_[reversed_index]);
	}
	reversed_hash_tree_ = SegmentTree<HashType>(prefix_hash);

	// count prefix_hash
	for (size_t prefix_index = 1; prefix_index < prefix_hash.size(); ++prefix_index) {
		prefix_hash[prefix_index] += prefix_hash[prefix_index - 1];
	}
}

bool StringHash::isPalindromicSubstring(int left_index, int right_index) {
	HashType direct_hash = direct_hash_tree_.rangeSum(left_index, right_index);
	HashType reverse_hash = reversed_hash_tree_.rangeSum(getReversedIndex(right_index), getReversedIndex(left_index));
	return isEqualHash(direct_hash, left_index, reverse_hash, getReversedIndex(right_index));
}

void StringHash::changeChar(int index, char new_char) {
	sample_[index] = new_char;
	HashType new_char_hash = countCharHash(index, new_char);
	direct_hash_tree_.updateElement(index, new_char_hash);
	index = getReversedIndex(index);
	new_char_hash = countCharHash(index, new_char);
	reversed_hash_tree_.updateElement(index, new_char_hash);
}

HashType StringHash::countCharHash(int index, char new_char) const {
	return prime_powers_[index] * static_cast<int>(new_char) % modulus_;
}

int StringHash::getReversedIndex(int index) const {
	return sample_.size() - index - 1;
}

bool StringHash::isEqualHash(HashType first_hash, int first_index, HashType second_hash, int second_index) const {
	int index_diff = second_index - first_index;
	if ((index_diff >= 0 && first_hash * prime_powers_[index_diff] == second_hash) ||
			(index_diff < 0 && second_hash * prime_powers_[-index_diff] == first_hash)) {
		return true;
	}
	return false;
}

