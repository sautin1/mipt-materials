#include <algorithm>
#include <iostream>
#include <string>

#include <stdexcept>
#include <string>
#include <vector>

typedef unsigned long long HashType;

class SegmentTree {
public:
	SegmentTree() = default;

	SegmentTree(const std::vector<HashType>& values)
		: size_(values.size()) {
		tree_.push_back(Node(0, 0, values.size()-1, HashType()));
		buildTree(values, 0);
	}

	HashType rangeSum(int left_bound, int right_bound) {
		return rangeSum(0, left_bound, right_bound);
	}

	void updateElement(int index, HashType new_value) {
		updateElement(0, index, new_value);
	}

private:
	struct Node {
		int left_son;
		int left_bound, right_bound;
		HashType value;
		Node(int _left_son, int _left_bound, int _right_bound, const HashType& _value)
			: left_son(_left_son), left_bound(_left_bound), right_bound(_right_bound), value(_value) {}
		Node() = default;
	};

	void buildTree(const std::vector<HashType>& values, int node_index) {
		if (tree_[node_index].left_bound == tree_[node_index].right_bound) {
			tree_[node_index].value = values[tree_[node_index].left_bound];
			return;
		}
		int middle_bound = (tree_[node_index].left_bound + tree_[node_index].right_bound) / 2;
		tree_[node_index].left_son = tree_.size();
		tree_.push_back(Node(0, tree_[node_index].left_bound, middle_bound, HashType()));
		tree_.push_back(Node(0, middle_bound + 1, tree_[node_index].right_bound, HashType()));
		buildTree(values, tree_[node_index].left_son);
		buildTree(values, tree_[node_index].left_son + 1);

		int left_son = tree_[node_index].left_son;
		tree_[node_index].value = tree_[left_son].value + tree_[left_son + 1].value;
	}

	HashType rangeSum(int node_index, int left_bound, int right_bound) {
		if (left_bound > right_bound) {
			return 0;
		}
		if ((tree_[node_index].left_bound == left_bound) && (tree_[node_index].right_bound == right_bound)) {
			return tree_[node_index].value;
		}
		int middle_bound = (tree_[node_index].left_bound + tree_[node_index].right_bound) / 2;
		HashType result = rangeSum(tree_[node_index].left_son, left_bound, std::min(middle_bound, right_bound));
		result += rangeSum(tree_[node_index].left_son + 1, std::max(left_bound, middle_bound + 1), right_bound);
		return result;
	}

	void updateElement(int node_index, int index, const HashType& new_value) {
		if (tree_[node_index].left_bound == tree_[node_index].right_bound) {
			tree_[node_index].value = new_value;
		} else {
			int middle_bound = (tree_[node_index].left_bound + tree_[node_index].right_bound) / 2;
			if (index <= middle_bound) {
				updateElement(tree_[node_index].left_son, index, new_value);
			} else {
				updateElement(tree_[node_index].left_son + 1, index, new_value);
			}
			int left_son = tree_[node_index].left_son;
			tree_[node_index].value = tree_[left_son].value + tree_[left_son + 1].value;
		}
	}

	std::vector<Node> tree_;
	size_t size_;
};

const HashType MODULUS_NEAR_LONG_LONG_MAX = 18446744073709551557U;
const int DEFAULT_PRIME_BASE = 31;

class StringHash {
public:
	StringHash(const std::string& sample, int prime_base, HashType modulus);

	bool isPalindromicSubstring(int left_index, int right_index);
	void changeChar(int index, char new_char);
private:
	bool isEqualHash(HashType first_hash, int first_index, HashType second_hash, int second_index) const;

	int size_;
	std::vector<HashType> prime_powers_;
	HashType modulus_;
	SegmentTree direct_hash_tree_;
	SegmentTree reversed_hash_tree_;
};

StringHash::StringHash(const std::string& sample, int prime_base, HashType modulus)
	: size_(sample.size()), prime_powers_(sample.size(), 0),
	  modulus_(modulus) {
	std::vector<HashType> prefix_hash(sample.size(), 0);
	// count prime_base powers
	prime_powers_[0] = 1;
	for (size_t prime_power = 1; prime_power < sample.size(); ++prime_power) {
		prime_powers_[prime_power] = prime_powers_[prime_power - 1] * prime_base % modulus;
	}

	// count char hashes
	for (size_t prefix_index = 0; prefix_index < sample.size(); ++prefix_index) {
		prefix_hash[prefix_index] = prime_powers_[prefix_index] * sample[prefix_index];
	}

	// create segment trees
	direct_hash_tree_ = SegmentTree(prefix_hash);
	for (size_t prefix_index = 0; prefix_index < sample.size(); ++prefix_index) {
		int reversed_index = sample.size() - prefix_index - 1;
		prefix_hash[prefix_index] = prime_powers_[prefix_index] * sample[reversed_index] % modulus_;
	}
	reversed_hash_tree_ = SegmentTree(prefix_hash);

	// count prefix_hash
	for (size_t prefix_index = 1; prefix_index < prefix_hash.size(); ++prefix_index) {
		prefix_hash[prefix_index] += prefix_hash[prefix_index - 1];
	}
}

bool StringHash::isPalindromicSubstring(int left_index, int right_index) {
	HashType direct_hash = direct_hash_tree_.rangeSum(left_index, right_index);
	HashType reverse_hash = reversed_hash_tree_.rangeSum(size_ - right_index - 1, size_ - left_index - 1);
	return isEqualHash(direct_hash, left_index, reverse_hash, size_ - right_index - 1);
}

void StringHash::changeChar(int index, char new_char) {
	HashType new_char_hash = prime_powers_[index] * new_char % modulus_;
	direct_hash_tree_.updateElement(index, new_char_hash);
	index = size_ - index - 1;
	new_char_hash = prime_powers_[index] * new_char % modulus_;
	reversed_hash_tree_.updateElement(index, new_char_hash);
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
	StringHash string_hash(sample, DEFAULT_PRIME_BASE, MODULUS_NEAR_LONG_LONG_MAX);

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
