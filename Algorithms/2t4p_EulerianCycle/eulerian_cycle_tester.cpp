#include "eulerian_cycle_tester.h"

node_t get_random_neighbour(const size_t node_quantity, const node_t current_node)
{
	node_t neighbour = rand() % node_quantity;
	while (neighbour == current_node){
		neighbour = rand() % node_quantity;
	}
	return neighbour;
}


void tester()
{
	size_t node_quantity = (rand() % (MAX_NODE_QUANTITY - MIN_NODE_QUANTITY + 1)) + MIN_NODE_QUANTITY;
	size_t edge_quantity = std::min((rand() % (node_quantity * node_quantity)) + 1, MIN_EDGE_QUANTITY);
	//std::cout << node_quantity << " " << edge_quantity << "\n";
	std::vector<edge_t> edges;
	std::multiset<edge_t, EdgeComparator> edges_multiset;
	node_t start_node = rand() % node_quantity;
	node_t current_node = start_node;
	for (size_t edge_index = 0; edge_index < edge_quantity-1; ++edge_index){
		node_t neighbour = get_random_neighbour(node_quantity, current_node);
		edge_t new_edge = std::make_pair(current_node, neighbour);
		edges.push_back(new_edge);
		edges_multiset.insert(new_edge);
		current_node = neighbour;
	}
	if (current_node == start_node){
		node_t neighbour = get_random_neighbour(node_quantity, current_node);
		edge_t new_edge = std::make_pair(current_node, neighbour);
		edges.push_back(new_edge);
		edges_multiset.insert(new_edge);
		current_node = neighbour;
		++edge_quantity;
	}
	edge_t new_edge = std::make_pair(current_node, start_node);
	edges.push_back(new_edge);
	edges_multiset.insert(new_edge);

	directed_graph graph(node_quantity, edges);
	std::vector<node_t> eul_cycle;

	bool success = graph.eulerian_cycle(eul_cycle, start_node);
	if (!success){
		throw std::runtime_error(std::string("Boolean answer is wrong!"));
	}

	for (size_t eul_cycle_index = 1; eul_cycle_index < eul_cycle.size(); ++eul_cycle_index){
		edge_t new_edge = std::make_pair(eul_cycle[eul_cycle_index-1], eul_cycle[eul_cycle_index]);
		std::multiset<edge_t, EdgeComparator>::iterator delete_it = edges_multiset.find(new_edge);
		if (delete_it == edges_multiset.end()){
			throw std::runtime_error(std::string("Not existing edge used!"));
		}
		edges_multiset.erase(delete_it);
	}
	if (!edges_multiset.empty()){
		throw std::runtime_error(std::string("Not all the edges are used!"));
	}
}

void test_eulerian_cycle(size_t test_quantity)
{
	srand(time(0));
	for (size_t test_index = 0; test_index < test_quantity; ++test_index){
		tester();
		std::cout << "Test #" << test_index+1 << " passed!\n";
	}
	std::cout << "All tests passed!\n";
}

