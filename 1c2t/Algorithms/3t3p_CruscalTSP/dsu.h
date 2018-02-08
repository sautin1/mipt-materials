#pragma once
#include <vector>
#include <algorithm>

class dsu
{
	size_t size_; //number of components
	std::vector<ssize_t> parent_;
	std::vector<size_t> set_rank_;
public:
	dsu(size_t size);
	size_t find_set(const size_t node);
	bool unite(size_t node1, size_t node2);
	size_t size() const;
};
