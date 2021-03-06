#include <fstream>
#include <iostream>
#include <vector>
#include <stdexcept>
#include <stack>
#include <cstdlib>

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
	directed_graph();
	directed_graph(size_t node_quantity, const std::vector<edge_t>& edges);
	directed_graph(size_t node_quantity, const std::vector< std::vector<bool> >& matrix);
	size_t size() const;
	bool dfs_nr(std::vector<size_t>& node_color, node_t start_node, size_t mode, std::vector<node_t>& node_toposort) const;
	bool dfs_caller() const;
	bool toposort(std::vector<node_t>& node_toposort) const;
	void transpose(directed_graph& new_graph) const;
	size_t scc(std::vector<size_t>& scc_number) const;
};


directed_graph::directed_graph(){}

directed_graph::directed_graph(size_t node_quantity, const std::vector<edge_t>& edges)
	:graph_(node_quantity, std::vector<node_t>()),
	  node_quantity_(node_quantity)
{
	for (std::vector<edge_t>::const_iterator edge_it = edges.begin(); edge_it != edges.end(); ++edge_it){
		graph_[edge_it->first].push_back(edge_it->second);
	}
}

directed_graph::directed_graph(size_t node_quantity, const std::vector< std::vector<bool> >& matrix)
	:graph_(node_quantity, std::vector<node_t>()),
	  node_quantity_(node_quantity)
{
	for (node_t node1 = 0; node1 < node_quantity; ++node1){
		for (node_t node2 = 0; node2 < node_quantity; ++node2){
			if (matrix[node1][node2]){
				graph_[node1].push_back(node2);
			}
		}
	}
}

size_t directed_graph::size() const
{
	return node_quantity_;
}

struct dfs_stack_item{
	dfs_stack_item(node_t new_node, size_t neighbour): node(new_node), next_neighbour_index(neighbour){}
	node_t node;
	size_t next_neighbour_index;
};

bool directed_graph::dfs_nr(std::vector<size_t>& node_color, node_t start_node, size_t mode, std::vector<node_t>& node_toposort) const
{
	/*returns true, if there are no loops in graph*/
	bool success = true;
	std::stack<dfs_stack_item> dfs_stack;
	dfs_stack.push(dfs_stack_item(start_node, 0));
	while (!dfs_stack.empty()){
		node_t current_node = dfs_stack.top().node;
		node_color[current_node] = NODE_GREY;
		if (mode == DFS_MODE){
			std::cout << "Entered " << current_node << "\n";
		}
		size_t neighbour_index = dfs_stack.top().next_neighbour_index;

		while (neighbour_index < graph_[current_node].size() && node_color[graph_[current_node][neighbour_index]] != NODE_WHITE){
			success = success && node_color[graph_[current_node][neighbour_index]] != NODE_GREY;
			++neighbour_index;
		}
		if (neighbour_index >= graph_[current_node].size()){
			if (mode == TOPOSORT_MODE){
				node_toposort.push_back(current_node);
			} else if (mode == DFS_MODE){
				std::cout << "Exited " << current_node << "\n";
			}
			node_color[current_node] = NODE_BLACK;
			dfs_stack.pop();
		} else {
			dfs_stack.pop();
			dfs_stack.push(dfs_stack_item(current_node, neighbour_index+1));
			dfs_stack.push(dfs_stack_item(graph_[current_node][neighbour_index], 0));
		}
	}
	return success;
}

bool directed_graph::toposort(std::vector<node_t>& node_toposort) const
{
	bool success = true;
	std::vector<size_t> node_color(node_quantity_, NODE_WHITE);
	for (node_t node = 0; node < node_quantity_; ++node){
		if (node_color[node] == NODE_WHITE){
			success = dfs_nr(node_color, node, TOPOSORT_MODE, node_toposort) && success;
		}
	}
	//reverse the vector of toposorted nodes
	for (size_t node_index = 0; node_index < node_toposort.size()/2; ++node_index){
		std::swap<node_t>(node_toposort[node_index], node_toposort[node_toposort.size()-node_index-1]);
	}
	return success;
}

bool directed_graph::dfs_caller() const
{
	/*returns true, if there are no loops in graph*/
	bool success = true;
	std::vector<size_t> node_color(node_quantity_, NODE_WHITE);
	std::vector<node_t> node_toposort;
	for (node_t node = 0; node < node_quantity_; ++node){
		if (node_color[node] == NODE_WHITE){
			success = dfs_nr(node_color, node, DFS_MODE, node_toposort) && success;
		}
	}
	return success;
}

void directed_graph::transpose(directed_graph& transposed_graph) const
{
	std::vector<edge_t> edges;
	for (node_t node = 0; node < node_quantity_; ++node){
		for (size_t neighbour_index = 0; neighbour_index < graph_[node].size(); ++neighbour_index){
			node_t neighbour = graph_[node][neighbour_index];
			edges.push_back(std::make_pair(neighbour, node));
		}
	}
	directed_graph new_graph(node_quantity_, edges);
	std::swap<directed_graph>(new_graph, transposed_graph);
}

size_t directed_graph::scc(std::vector<size_t>& scc_number) const
{
	//Kosaraju's algorithm
	scc_number.assign(node_quantity_, 0);
	std::vector<node_t> node_toposort;
	toposort(node_toposort);
	directed_graph transposed_graph;
	transpose(transposed_graph);
	std::vector<size_t> node_color(node_quantity_, NODE_WHITE);

	size_t strong_component_quantity = 0;
	for (size_t node_index = 0; node_index < node_quantity_; ++node_index){
		if (node_color[node_toposort[node_index]] == NODE_WHITE) {
			std::vector<node_t> weak_component;
			transposed_graph.dfs_nr(node_color, node_toposort[node_index], TOPOSORT_MODE, weak_component);
			++strong_component_quantity;
			for (size_t component_node = 0; component_node < weak_component.size(); ++component_node){
				scc_number[weak_component[component_node]] = strong_component_quantity;
			}
		}
	}
	return strong_component_quantity;
}


const std::string INPUT_FILE  = "input.txt";
const std::string OUTPUT_FILE = "output.txt";

int main()
{
	std::ifstream fin;
	fin.open(INPUT_FILE.c_str(), std::ifstream::in);
	if (!fin.good()){
		throw std::runtime_error(std::string("Cannot open input file: ") + INPUT_FILE);
	}
	size_t node_quantity, edge_quantity;
	std::vector<edge_t> edges;
	fin >> node_quantity >> edge_quantity;
	for (size_t edge_index = 0; edge_index < edge_quantity; ++edge_index){
		size_t node1, node2;
		fin >> node1 >> node2;
		edges.push_back(std::make_pair(node1-1, node2-1));
	}
	fin.close();
	directed_graph graph(node_quantity, edges);
	std::vector<size_t> scc;
	size_t scc_quantity = graph.scc(scc);

	std::ofstream fout;
	fout.open(OUTPUT_FILE.c_str(), std::ofstream::out);
	fout << scc_quantity << "\n";
	for (size_t node_index = 0; node_index < node_quantity; ++node_index){
		fout << scc[node_index] << " ";
	}
	fout.close();

	return 0;
}
