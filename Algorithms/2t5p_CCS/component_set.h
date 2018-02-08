#pragma once
#include <vector>
#include <algorithm>

class component_set
{
	std::vector<ssize_t> parent_;
	std::vector<size_t> component_rank_;
	size_t size_; //number of components
public:
	component_set(size_t node_quantity);
	size_t find_set(const size_t node);
	void unite(size_t node1, size_t node2);
	size_t size() const;
};
