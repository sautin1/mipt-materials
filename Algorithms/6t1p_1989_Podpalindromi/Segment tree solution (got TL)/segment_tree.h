#ifndef SEGMENT_TREE_H
#define SEGMENT_TREE_H

#include <iostream>
#include <stdexcept>
#include <vector>

void checkIndices(int left_index, int right_index, int max_index);

template <typename ValueType>
class SegmentTree {
public:
	SegmentTree() = default;

	//SegmentTree(const SegmentTree& other) = default;

	SegmentTree(const std::vector<ValueType>& values)
		: size_(values.size()) {
		tree_.push_back(Node(0, 0, values.size()-1, ValueType()));
		buildTree(values, 0);
	}

	ValueType rangeSum(int left_bound, int right_bound) {
		checkIndices(left_bound, right_bound, size_ - 1);
		return rangeSum(0, left_bound, right_bound);
	}

	void updateElement(int index, ValueType new_value) {
		checkIndices(index, index, size_ - 1);
		updateElement(0, index, new_value);
	}

	void updateSegment(int left_bound, int right_bound, const ValueType& modification) {
		checkIndices(left_bound, right_bound, size_ - 1);
		updateSegment(0, left_bound, right_bound, modification);
	}

private:
	struct Node {
		int left_son;
		int left_bound, right_bound;
		ValueType value;
		ValueType modification;
		Node(int _left_son, int _left_bound, int _right_bound, const ValueType& _value)
			: left_son(_left_son), left_bound(_left_bound), right_bound(_right_bound), value(_value), modification() {}
		Node() = default;
	};

	void buildTree(const std::vector<ValueType>& values, int node_index) {
		if (tree_[node_index].left_bound == tree_[node_index].right_bound) {
			tree_[node_index].value = values[tree_[node_index].left_bound];
			return;
		}
		int middle_bound = (tree_[node_index].left_bound + tree_[node_index].right_bound) / 2;
		tree_[node_index].left_son = tree_.size();
		tree_.push_back(Node(0, tree_[node_index].left_bound, middle_bound, ValueType()));
		tree_.push_back(Node(0, middle_bound + 1, tree_[node_index].right_bound, ValueType()));
		buildTree(values, tree_[node_index].left_son);
		buildTree(values, tree_[node_index].left_son + 1);

		updateNodeValue(node_index);
	}

	ValueType rangeSum(int node_index, int left_bound, int right_bound) {
		if (left_bound > right_bound) {
			return 0;
		}
		if ((tree_[node_index].left_bound == left_bound) && (tree_[node_index].right_bound == right_bound)) {
			return tree_[node_index].value;
		}
		pushModification(node_index);
		int middle_bound = (tree_[node_index].left_bound + tree_[node_index].right_bound) / 2;
		ValueType result = rangeSum(tree_[node_index].left_son, left_bound, std::min(middle_bound, right_bound));
		result += rangeSum(tree_[node_index].left_son + 1, std::max(left_bound, middle_bound + 1), right_bound);
		result += tree_[node_index].modification;
		return result;
	}

	void updateElement(int node_index, int index, const ValueType& new_value) {
		if (tree_[node_index].left_bound == tree_[node_index].right_bound) {
			tree_[node_index].value = new_value;
		} else {
			pushModification(node_index);
			int middle_bound = (tree_[node_index].left_bound + tree_[node_index].right_bound) / 2;
			if (index <= middle_bound) {
				updateElement(tree_[node_index].left_son, index, new_value);
			} else {
				updateElement(tree_[node_index].left_son + 1, index, new_value);
			}
			updateNodeValue(node_index);
		}
	}

	void updateSegment(int node_index, int left_bound, int right_bound, const ValueType& modification) {
		if (left_bound > right_bound) {
			return;
		}
		if ((tree_[node_index].left_bound == left_bound) && (tree_[node_index].right_bound == right_bound)) {
			tree_[node_index].modification += modification;
			tree_[node_index].value += modification * (right_bound - left_bound + 1);
		} else {
			pushModification(node_index);
			int middle_bound = (tree_[node_index].left_bound + tree_[node_index].right_bound) / 2;
			updateSegment(tree_[node_index].left_son, left_bound, std::min(middle_bound, right_bound), modification);
			updateSegment(tree_[node_index].left_son + 1, std::max(left_bound, middle_bound + 1), right_bound, modification);
			updateNodeValue(node_index);
		}
	}

	void updateNodeValue(int node_index) {
		int left_son = tree_[node_index].left_son;
		tree_[node_index].value = tree_[left_son].value + tree_[left_son + 1].value;
	}

	void pushModification(int node_index) {
		if (tree_[node_index].modification == ValueType()) {
			return;
		}
		int left_son = tree_[node_index].left_son;

		int son_range_length = tree_[left_son].right_bound - tree_[left_son].left_bound + 1;
		tree_[left_son].modification += tree_[node_index].modification;
		tree_[left_son].value += tree_[node_index].modification * son_range_length;

		son_range_length = tree_[left_son + 1].right_bound - tree_[left_son + 1].left_bound + 1;
		tree_[left_son + 1].modification += tree_[node_index].modification * son_range_length;
		tree_[left_son + 1].value += tree_[node_index].modification * son_range_length;

		tree_[node_index].modification = ValueType();
	}

	std::vector<Node> tree_;
	size_t size_;
};

#endif // SEGMENT_TREE_H
