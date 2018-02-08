#include <fstream>
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
	//directed_graph(size_t node_quantity, const std::vector<edge_t>& edges);
	directed_graph(size_t node_quantity, const std::vector< std::vector<bool> >& matrix);
	//size_t size() const;
	size_t dfs_nr(std::vector<size_t>& node_color, node_t start_node, size_t mode, std::vector<node_t>& node_toposort) const;
	//bool dfs_caller() const;
	//bool toposort(std::vector<node_t>& node_toposort) const;
};


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

/*size_t directed_graph::size() const
{
	return node_quantity_;
}*/

struct dfs_stack_item{
	dfs_stack_item(node_t new_node, size_t neighbour): node(new_node), next_neighbour_index(neighbour){}
	node_t node;
	size_t next_neighbour_index;
};

size_t directed_graph::dfs_nr(std::vector<size_t>& node_color, node_t start_node, size_t mode, std::vector<node_t>& node_toposort) const
{
	/*returns the number of nodes in start_node's connected component*/
	size_t result = 0;
	std::stack<dfs_stack_item> dfs_stack;
	dfs_stack.push(dfs_stack_item(start_node, 0));
	while (!dfs_stack.empty()){
		node_t current_node = dfs_stack.top().node;
		node_color[current_node] = NODE_GREY;
		if (mode == DFS_MODE){
			//std::cout << "Entered " << current_node << "\n";
		}
		size_t neighbour_index = dfs_stack.top().next_neighbour_index;

		while (neighbour_index < graph_[current_node].size() && node_color[graph_[current_node][neighbour_index]] != NODE_WHITE){
			++neighbour_index;
		}
		if (neighbour_index >= graph_[current_node].size()){
			if (mode == TOPOSORT_MODE){
				node_toposort.push_back(current_node);
			} else {
				//std::cout << "Exited " << current_node << "\n";
				++result;
			}
			node_color[current_node] = NODE_BLACK;
			dfs_stack.pop();
		} else {
			dfs_stack.pop();
			dfs_stack.push(dfs_stack_item(current_node, neighbour_index+1));
			dfs_stack.push(dfs_stack_item(graph_[current_node][neighbour_index], 0));
		}
	}
	return result;
}

/*bool directed_graph::toposort(std::vector<node_t>& node_toposort) const
{
	bool success = true;
	std::vector<size_t> node_color(node_quantity_, NODE_WHITE);
	for (node_t node = 0; node < node_quantity_; ++node){
		if (node_color[node] == NODE_WHITE){
			success = success && dfs_nr(node_color, node, TOPOSORT_MODE, node_toposort);
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
	//returns true, if there are no loops in graph
	bool success = true;
	std::vector<size_t> node_color(node_quantity_, NODE_WHITE);
	std::vector<node_t> node_toposort;
	for (node_t node = 0; node < node_quantity_; ++node){
		if (node_color[node] == NODE_WHITE){
			success = success && dfs_nr(node_color, node, DFS_MODE, node_toposort);
		}
	}
	return success;
}*/

const std::string INPUT_FILE  = "input.txt";
const std::string OUTPUT_FILE = "output.txt";

int main()
{
	std::ifstream fin;
	fin.open(INPUT_FILE.c_str(), std::ifstream::in);
	if (!fin.good()){
		throw std::runtime_error(std::string("Cannot open input file: ") + INPUT_FILE);
	}
	size_t node_quantity, start_node;
	fin >> node_quantity >> start_node;
	--start_node;
	std::vector< std::vector<bool> > matrix(node_quantity, std::vector<bool>(node_quantity, false));
	for (size_t node1 = 0; node1 < node_quantity; ++node1){
		for (size_t node2 = 0; node2 < node_quantity; ++node2){
			bool isEdge;
			fin >> isEdge;
			matrix[node1][node2] = isEdge;
		}
	}
	fin.close();
	directed_graph graph(node_quantity, matrix);
	std::vector<node_t> node_toposort;
	std::vector<size_t> node_color(node_quantity, NODE_WHITE);
	size_t result = graph.dfs_nr(node_color, start_node, DFS_MODE, node_toposort);

	std::ofstream fout;
	fout.open(OUTPUT_FILE.c_str(), std::ofstream::out);
	fout << result;
	fout.close();

	return 0;
}
