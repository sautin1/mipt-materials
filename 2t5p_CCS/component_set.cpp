#include "component_set.h"

component_set::component_set(size_t node_quantity)
{
	component_rank_.assign(node_quantity, 1);
	parent_.assign(node_quantity, -1);
	size_ = node_quantity;
}

size_t component_set::find_set(const size_t node)
{
	size_t root;
	if (parent_[node] == -1){
		root = node;
	}
	else {
		root = find_set(parent_[node]);
		parent_[node] = root;
	}
	return root;
}

void component_set::unite(size_t node1, size_t node2)
{
	size_t root1 = find_set(node1);
	size_t root2 = find_set(node2);
	if (root1 == root2){
		return;
	}
	if (component_rank_[root1] > component_rank_[root2]){
		std::swap<size_t>(node1, node2);
		std::swap<size_t>(root1, root2);
	}

	//Add component1 to component2 root
	parent_[root1] = root2;
	if (component_rank_[root1] == component_rank_[root2]){
		++component_rank_[root2];
	}
	--size_;
}

size_t component_set::size() const
{
	return size_;
}
