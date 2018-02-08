#pragma once
#include <ctime>
#include <set>
#include <cstdlib>
#include "directed_graph.h"

const size_t MAX_NODE_QUANTITY = 1000;
const size_t MIN_NODE_QUANTITY = 5;
const size_t MIN_EDGE_QUANTITY = 50000;
const size_t TEST_QUANTITY = 1000;

struct EdgeComparator{
	bool operator()(const edge_t left, const edge_t right)
	{
		return (left.first < right.first) || ((left.first == right.first) && (left.second < right.second));
	}
};

node_t get_random_neighbour(const size_t node_quantity, const node_t current_node);
void tester();
void test_eulerian_cycle(size_t test_quantity);
