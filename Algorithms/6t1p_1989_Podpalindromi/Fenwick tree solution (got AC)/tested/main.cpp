#include <algorithm>
#include <iostream>
#include <string>

#include <stdexcept>
#include <string>
#include <vector>

template <typename ValueType>
class FenwickTree {
public:
	FenwickTree() = default;
	explicit FenwickTree(int size) : tree_(size, ValueType()) {}
	explicit FenwickTree(const std::vector<ValueType>& values) : tree_(values.size(), ValueType()) {
		for (size_t value_index = 0; value_index < values.size(); ++value_index) {
			update(value_index, values[value_index]);
		}
	}

	void increase(int index, ValueType delta) {
		for (; index < static_cast<int>(tree_.size()); index = (index | (index + 1))) {
			tree_[index] += delta;
		}
	}

	void update(int index, ValueType new_value) {
		ValueType delta = new_value - rangeSum(index, index);
		increase(index, delta);
	}

	ValueType rangeSum(int left_index, int right_index) {
		return rangeSum(right_index) - rangeSum(left_index-1);
	}
private:
	ValueType rangeSum(int index) {
		ValueType result = 0;
		for (; index >= 0; index = (index & (index + 1)) - 1) {
			result += tree_[index];
		}
		return result;
	}

	std::vector<ValueType> tree_;
};

typedef long long HashType;

const int DEFAULT_PRIME_BASE = 31;

class StringHash {
public:
	StringHash(const std::string& sample, int prime_base);

	bool isPalindromicSubstring(int left_index, int right_index);
	void changeChar(int index, char new_char);
private:
	bool isEqualHash(HashType first_hash, int first_index, HashType second_hash, int second_index) const;
	HashType countCharHash(int index, char new_char) const;
	int getReversedIndex(int index) const;

	std::string sample_;
	std::vector<HashType> prime_powers_;
	FenwickTree<HashType> direct_hash_tree_;
	FenwickTree<HashType> reversed_hash_tree_;
};

StringHash::StringHash(const std::string& sample, int prime_base)
	: sample_(sample), prime_powers_(sample.size(), 0) {
	std::vector<HashType> prefix_hash(sample.size(), 0);
	// count prime_base powers
	prime_powers_[0] = 1;
	for (size_t prime_power = 1; prime_power < sample_.size(); ++prime_power) {
		prime_powers_[prime_power] = prime_powers_[prime_power - 1] * prime_base;
	}

	// count char hashes
	for (size_t prefix_index = 0; prefix_index < sample_.size(); ++prefix_index) {
		prefix_hash[prefix_index] = countCharHash(prefix_index, sample_[prefix_index]);
	}

	// create fenwick trees
	direct_hash_tree_ = FenwickTree<HashType>(prefix_hash);
	for (size_t prefix_index = 0; prefix_index < sample_.size(); ++prefix_index) {
		int reversed_index = getReversedIndex(prefix_index);
		prefix_hash[prefix_index] = countCharHash(prefix_index, sample_[reversed_index]);
	}
	reversed_hash_tree_ = FenwickTree<HashType>(prefix_hash);

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
	direct_hash_tree_.update(index, new_char_hash);
	index = getReversedIndex(index);
	new_char_hash = countCharHash(index, new_char);
	reversed_hash_tree_.update(index, new_char_hash);
}

HashType StringHash::countCharHash(int index, char new_char) const {
	return prime_powers_[index] * (new_char - 'a');
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

int main() {
	std::ios_base::sync_with_stdio(false);
	std::string sample;
	std::getline(std::cin, sample);
	StringHash string_hash(sample, DEFAULT_PRIME_BASE);

	int query_quantity;
	std::cin >> query_quantity;
	for (int query_index = 0; query_index < query_quantity; ++query_index) {
		std::string query_name;
		std::cin >> query_name;
		if (query_name[0] == 'p') {
			int left_index, right_index;
			std::cin >> left_index >> right_index;
			if (string_hash.isPalindromicSubstring(left_index-1, right_index-1)) {
				std::cout << "Yes\n";
			} else {
				std::cout << "No\n";
			}
		} else if (query_name[0] == 'c') {
			int index;
			char new_char;
			std::cin >> index >> new_char;
			string_hash.changeChar(index-1, new_char);
		}
	}
	return 0;
}
