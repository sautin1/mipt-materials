#pragma once
#include <iostream>
#include <vector>
#include <stdexcept>
#include <cstdlib> //for ssize_t
#include <stack>

const size_t NODE_WHITE = 0;
const size_t NODE_GREY  = 1;
const size_t NODE_BLACK = 2;

const size_t DFS_MODE = 0;
const size_t TOPOSORT_MODE = 1;

const size_t NON_COMPONENT_MODE = 0;
const size_t COMPONENT_MODE = 1;

typedef size_t node_t;

struct neighbour_t{
	neighbour_t(node_t _node, size_t _edge_index)
		: node(_node), edge_index(_edge_index){}
	node_t node;
	size_t edge_index;
};

typedef std::vector< neighbour_t > neighbours_vector_t;

struct edge_t{
	edge_t(node_t _first, node_t _second, size_t _edge_index)
		: first(_first), second(_second), edge_index(_edge_index){}
	node_t first;
	node_t second;
	size_t edge_index;
};

struct dfs_stack_item{
	dfs_stack_item(node_t new_node, size_t new_neighbour_index)
		: node(new_node), next_neighbour_index(new_neighbour_index){}
	node_t node;
	size_t next_neighbour_index;
};

class undirected_graph
{
	std::vector< neighbours_vector_t > graph_;
	size_t edge_quantity_;
	size_t node_quantity_;
public:
	undirected_graph(size_t node_quantity, size_t edge_quantity, const std::vector<edge_t>& edges);
	void bridges(std::vector<size_t>& bridges, size_t mode, std::vector<size_t>& edge_component) const;
	void cut_vertices(std::vector<node_t>& cut_vertices, size_t mode, std::vector<ssize_t> &node_component) const;
	ssize_t get_parent(std::stack<dfs_stack_item>& dfs_stack) const;
	size_t size() const;
	size_t linked_components(ssize_t excluded_node) const;
};
