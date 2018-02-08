#pragma once
#include <iostream>
#include <vector>
#include <stdexcept>
#include <limits>
#include <algorithm>
#include <cmath>
#include <stack>
#include "dsu.h"

typedef size_t node_t;

//Weighted graph
typedef double weight_t;
struct edge_t
{
	edge_t(node_t first_, node_t second_, weight_t weight_): first(first_), second(second_), weight(weight_){}
	bool operator<(const edge_t& edge) const{
		return (weight < edge.weight);
	}
	node_t first;
	node_t second;
	weight_t weight;
};
const weight_t INF = (std::numeric_limits<weight_t>().max() / 2) - 1;


class weighted_graph
{
	std::vector< edge_t > edges_;
	size_t node_quantity_;
public:
	weighted_graph();
	weighted_graph(size_t node_quantity, const std::vector<edge_t>& edges);
	weighted_graph(size_t node_quantity, const std::vector< std::vector<weight_t> >& matrix);
	size_t size() const;
	void MST(std::vector<edge_t>& tree_edges); //Cruscal
	void TSP_path(node_t start_node, std::vector<node_t>& path);
};

//Undirected graph

struct dfs_stack_item{
	dfs_stack_item(node_t new_node, size_t neighbour): node(new_node), next_neighbour_index(neighbour){}
	node_t node;
	size_t next_neighbour_index;
};

class undirected_graph
{
	std::vector< std::vector<node_t> > graph_;
	size_t node_quantity_;
public:
	undirected_graph(size_t node_quantity, const std::vector<edge_t>& edges);
	void dfs_tin(node_t start_node, std::vector<node_t>& tin) const;
	size_t size() const;
};
