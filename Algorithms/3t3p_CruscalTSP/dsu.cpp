#include "dsu.h"

dsu::dsu(size_t size)
	:size_(size), parent_(size, -1), set_rank_(size, 1)
{}

size_t dsu::find_set(const size_t node)
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

bool dsu::unite(size_t node1, size_t node2)
{
	size_t root1 = find_set(node1);
	size_t root2 = find_set(node2);
	if (root1 == root2){
		return false;
	}
	if (set_rank_[root1] > set_rank_[root2]){
		std::swap<size_t>(node1, node2);
		std::swap<size_t>(root1, root2);
	}

	//Add component1 to component2 root
	parent_[root1] = root2;
	if (set_rank_[root1] == set_rank_[root2]){
		++set_rank_[root2];
	}
	--size_;
	return true;
}

size_t dsu::size() const
{
	return size_;
}
