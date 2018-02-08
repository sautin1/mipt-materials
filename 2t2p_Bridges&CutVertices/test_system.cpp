#include "test_system.h"

void bridge_tester()
{
	/*tester complexity is O(E^2 + BlogB + E*V), where B - number of bridges*/
	//Generate random graph with loops and multiple edges
	size_t node_quantity = (rand() % (MAX_NODE_QUANTITY - MIN_NODE_QUANTITY + 1)) + MIN_NODE_QUANTITY;
	size_t edge_quantity = std::min((rand() % (node_quantity * (node_quantity-1) / 2)) + 1, MAX_EDGE_QUANTITY);
	std::cout << "N: " << node_quantity << ". E: " << edge_quantity << ". ";
	std::vector<edge_t> edges;

	for (size_t edge_index = 0; edge_index < edge_quantity; ++edge_index){
		node_t node1 = rand() % node_quantity;
		node_t node2 = rand() % node_quantity;
		edges.push_back(edge_t(node1, node2, edge_index));
	}
	undirected_graph graph(node_quantity, edge_quantity, edges);
	size_t component_quantity = graph.linked_components(-1);

	std::vector<size_t> bridges;
	std::vector<size_t> edge_component;
	graph.bridges(bridges, COMPONENT_MODE, edge_component);
	std::cout << "B: " << bridges.size() << "\n";
	std::multiset<size_t> bridges_multiset;
	for (size_t bridge_index = 0; bridge_index < bridges.size(); ++bridge_index){
		bridges_multiset.insert(bridges[bridge_index]);
	}

	/*test each edge if it is a bridge*/
	for (size_t edge_index = 0; edge_index < edge_quantity; ++edge_index){
		edge_t tested_edge = edges[edge_index];
		std::swap(edges[edge_index], edges.back());
		edges.pop_back();
		undirected_graph graph_copy(node_quantity, edge_quantity-1, edges);
		size_t new_component_quantity = graph_copy.linked_components(-1);
		if (new_component_quantity > component_quantity){
			//tested_edge is a bridge
			std::multiset<size_t>::iterator delete_it = bridges_multiset.find(tested_edge.edge_index);
			if (delete_it == bridges_multiset.end()){
				throw std::runtime_error(std::string("Not all the bridges found!"));
			}
			bridges_multiset.erase(delete_it);
		}
		edges.push_back(tested_edge);
		std::swap(edges[edge_index], edges.back());
	}
	if (!bridges_multiset.empty()){
		throw std::runtime_error(std::string("Too many bridges found!"));
	}

	//check edge_component
	bridges.clear();
	for (size_t component_index = 0; ; ++component_index){
		node_t component_node_quantity = 0;
		std::vector<ssize_t> node_new_index(node_quantity, -1);
		std::vector<edge_t> component_edges;
		for (size_t edge_index = 0; edge_index < edge_quantity; ++edge_index){
			if (edge_component[edges[edge_index].first ] == component_index &&
				edge_component[edges[edge_index].second] == component_index){
				if (node_new_index[edge_component[edges[edge_index].first]] == -1){
					++component_node_quantity;
					node_new_index[edge_component[edges[edge_index].first]] = component_node_quantity - 1;
				}
				if (node_new_index[edge_component[edges[edge_index].second]] == -1){
					++component_node_quantity;
					node_new_index[edge_component[edges[edge_index].second]] = component_node_quantity - 1;
				}
				component_edges.push_back(edge_t(node_new_index[edge_component[edges[edge_index].first]],
						node_new_index[edge_component[edges[edge_index].second]], edge_index));
			}
		}
		if (component_edges.size() == 0){
			break;
		}
		undirected_graph graph_component(component_node_quantity, component_edges.size(), component_edges);
		std::vector<size_t> edge_component_temp;
		graph_component.bridges(bridges, NON_COMPONENT_MODE, edge_component_temp);
		bool linked_component_quantity = (graph_component.linked_components(-1) == 1);
		if (!linked_component_quantity){
			throw std::runtime_error(std::string("Edge component is not linked!"));
		}
		if (!bridges.empty()){
			throw std::runtime_error(std::string("Edge component has bridges!"));
		}
	}

}

void cutvertices_tester()
{
	/*tester complexity is O(VE + VlogC)), where C - number of cut-vertices*/
	//Generate random graph with loops and multiple edges
	size_t node_quantity = (rand() % (MAX_NODE_QUANTITY - MIN_NODE_QUANTITY + 1)) + MIN_NODE_QUANTITY;
	size_t edge_quantity = std::min((rand() % (node_quantity * (node_quantity-1) / 2)) + 1, MAX_EDGE_QUANTITY);
	std::cout << "N: " << node_quantity << ". E: " << edge_quantity << ". ";
	std::vector<edge_t> edges;

	for (size_t edge_index = 0; edge_index < edge_quantity; ++edge_index){
		node_t node1 = rand() % node_quantity;
		node_t node2 = rand() % node_quantity;
		edges.push_back(edge_t(node1, node2, edge_index));
	}
	undirected_graph graph(node_quantity, edge_quantity, edges);
	size_t component_quantity = graph.linked_components(-1);

	std::vector<size_t> cut_vertices;
	std::vector<ssize_t> node_component;
	graph.cut_vertices(cut_vertices, COMPONENT_MODE, node_component);
	std::cout << "CV: " << cut_vertices.size() << "\n";
	std::multiset<size_t> cutvertices_multiset;
	for (size_t cutvertex_index = 0; cutvertex_index < cut_vertices.size(); ++cutvertex_index){
		cutvertices_multiset.insert(cut_vertices[cutvertex_index]);
	}

	/*test each node if it is a cut-vertex*/
	for (node_t node_index = 0; node_index < node_quantity; ++node_index){
		size_t new_component_quantity = graph.linked_components(node_index);
		if (new_component_quantity > component_quantity){
			//node_index is a cut-vertex
			std::multiset<size_t>::iterator delete_it = cutvertices_multiset.find(node_index);
			if (delete_it == cutvertices_multiset.end()){
				throw std::runtime_error(std::string("Not all the cut-vertices found!"));
			}
			cutvertices_multiset.erase(delete_it);
		}
	}
	if (!cutvertices_multiset.empty()){
		throw std::runtime_error(std::string("Too many cut-vertices found!"));
	}

	//check node_component
	cut_vertices.clear();
	for (size_t component_index = 0; ; ++component_index){
		node_t component_node_quantity = 0;
		std::vector<ssize_t> node_new_index(node_quantity, -1);
		std::vector<edge_t> component_edges;
		for (size_t edge_index = 0; edge_index < edge_quantity; ++edge_index){
			if (node_component[edges[edge_index].edge_index] == component_index){
				if (node_new_index[edges[edge_index].first] == -1){
					++component_node_quantity;
					node_new_index[edges[edge_index].first] = component_node_quantity - 1;
				}
				if (node_new_index[edges[edge_index].second] == -1){
					++component_node_quantity;
					node_new_index[edges[edge_index].second] = component_node_quantity - 1;
				}
				component_edges.push_back(edge_t(node_new_index[edges[edge_index].first],
						node_new_index[edges[edge_index].second], edge_index));
			}
		}
		if (component_edges.size() == 0){
			break;
		}
		undirected_graph graph_component(component_node_quantity, component_edges.size(), component_edges);
		std::vector<ssize_t> node_component_temp;
		graph_component.cut_vertices(cut_vertices, NON_COMPONENT_MODE, node_component_temp);
		bool linked_component_quantity = (graph_component.linked_components(-1) == 1);
		if (!linked_component_quantity){
			throw std::runtime_error(std::string("Node component is not linked!"));
		}
		if (!cut_vertices.empty()){
			throw std::runtime_error(std::string("Node component has cut vertices!"));
		}
	}


}

void test_bridges(size_t test_quantity)
{
	std::cout << "TEST BRIDGES!\n";
	srand(time(0));
	for (size_t test_index = 0; test_index < test_quantity; ++test_index){
		std::cout << "Test #" << test_index+1 << ". ";
		bridge_tester();
		std::cout << " OK!\n\n";
	}
	std::cout << "All tests passed!\n";
}

void test_bridges()
{
	test_bridges(TEST_QUANTITY);
}

void test_cutvertices(size_t test_quantity)
{
	srand(time(0));
	std::cout << "TEST CUT VERTICES!\n";
	for (size_t test_index = 0; test_index < test_quantity; ++test_index){
		std::cout << "Test #" << test_index+1 << ". ";
		cutvertices_tester();
		std::cout << " OK!\n\n";
	}
	std::cout << "All tests passed!\n";
}

void test_cutvertices()
{
	test_cutvertices(TEST_QUANTITY);
}
