#pragma once
#include <iostream>
#include <vector>
#include <stdexcept>
#include <limits>
#include <algorithm>
#include <cmath>

typedef size_t node_t;
typedef double weight_t;
struct edge_t{
	edge_t(node_t first_, node_t second_, weight_t weight_): first(first_), second(second_), weight(weight_){}
	node_t first;
	node_t second;
	weight_t weight;
};
const weight_t INF = (std::numeric_limits<weight_t>().max() / 2) - 1;
const weight_t EPS = 1.0E-14;

class weighted_graph
{
	std::vector< std::vector<weight_t> > matrix_;
	size_t node_quantity_;
	void getPath(node_t from, node_t to, std::vector< std::vector<node_t> >& reference, std::vector<node_t>& path);
public:
	weighted_graph();
	weighted_graph(size_t node_quantity, const std::vector<edge_t>& edges);
	weighted_graph(size_t node_quantity, const std::vector< std::vector<weight_t> >& matrix);
	size_t size() const;
	bool negative_cycle(std::vector<node_t>& cycle) const; //Warshall-Floyd
	void print_shortest_path(node_t from, node_t to, const std::vector< std::vector<ssize_t> >& reference,
							 std::vector<node_t>& path) const;
};

void preprocess(size_t node_quantity, std::vector< std::vector<weight_t> >& matrix);
