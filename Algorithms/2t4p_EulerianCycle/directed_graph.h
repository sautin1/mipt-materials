#pragma once
#include <iostream>
#include <vector>
#include <stdexcept>
#include <stack>

typedef size_t node_t;
typedef std::pair<node_t, node_t> edge_t;
typedef std::vector<node_t> neighbour_t;

class directed_graph
{
	std::vector< neighbour_t > graph_;
	size_t node_quantity_;
public:
	directed_graph(size_t node_quantity, const std::vector<edge_t>& edges);
	size_t size() const;
	size_t component_size_DFS(std::vector<bool>& visited, node_t node) const;
	bool one_weak_component() const;
	//bool is_connected() const;
	bool is_eulerian() const;
	bool eulerian_cycle(std::vector<node_t>& eul_cycle, node_t start_node) const;
};
