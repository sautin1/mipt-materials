#ifndef FENWICK_TREE_H
#define FENWICK_TREE_H

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

#endif // FENWICK_TREE_H
