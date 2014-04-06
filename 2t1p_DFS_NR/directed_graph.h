#pragma once
#include <iostream>
#include <vector>
#include <stdexcept>
#include <stack>

const size_t NODE_WHITE = 0;
const size_t NODE_GREY  = 1;
const size_t NODE_BLACK = 2;

const size_t DFS_MODE = 0;
const size_t TOPOSORT_MODE = 1;

typedef size_t node_t;
typedef std::pair<node_t, node_t> edge_t;
typedef std::vector<node_t> neighbour_t;

class directed_graph
{
	std::vector< neighbour_t > graph_;
	size_t node_quantity_;
public:
	directed_graph(size_t node_quantity, const std::vector<edge_t>& edges);
	directed_graph(size_t node_quantity, const std::vector< std::vector<bool> >& matrix);
	size_t size() const;
    bool dfs_nr(std::vector<size_t>& node_color, node_t start_node, size_t mode, std::vector<node_t>& node_toposort) const;
	bool dfs_caller() const;
	bool toposort(std::vector<node_t>& node_toposort) const;
};
