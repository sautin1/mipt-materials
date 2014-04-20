#include <fstream>
#include <iostream>
#include <vector>
#include <stdexcept>
#include <stack>
#include <algorithm>

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
	node_t get_parent(std::stack<dfs_stack_item>& dfs_stack) const;
	size_t size() const;
};

undirected_graph::undirected_graph(size_t node_quantity, size_t edge_quantity, const std::vector<edge_t>& edges)
	:graph_(node_quantity, neighbours_vector_t()), edge_quantity_(edge_quantity),
	  node_quantity_(node_quantity)
{
	for (std::vector<edge_t>::const_iterator edge_it = edges.begin(); edge_it != edges.end(); ++edge_it){
		graph_[edge_it->first].push_back(neighbour_t(edge_it->second, edge_it->edge_index));
		if (edge_it->first != edge_it->second) {
			graph_[edge_it->second].push_back(neighbour_t(edge_it->first, edge_it->edge_index));
		}
	}
}

size_t undirected_graph::size() const
{
	return node_quantity_;
}

node_t undirected_graph::get_parent(std::stack<dfs_stack_item>& dfs_stack) const
{
	//returns the parent node of dfs_stack.top().node (.node field of the pretop item in dfs_stack)
	node_t parent;
	if (dfs_stack.size() > 1){
		dfs_stack_item top_item = dfs_stack.top();
		dfs_stack.pop();
		parent = dfs_stack.top().node;
		dfs_stack.push(top_item);
	} else {
		parent = dfs_stack.top().node;
	}
	return parent;
}

void undirected_graph::bridges(std::vector<size_t>& bridges, size_t mode, std::vector<size_t>& edge_component) const
{
	bridges.clear();
	std::vector<size_t> node_color(node_quantity_, NODE_WHITE);
	std::vector<bool> parent_touched(node_quantity_, false); //is used only to process multiple edges
	std::stack<node_t> node_stack; //COMPONENT_MODE
	size_t edge_component_quantity = 0;
	/*if (mode == COMPONENT_MODE){
		edge_component.assign(node_quantity_, 0);
	}*/

	for (node_t start_node = 0; start_node < node_quantity_; ++start_node){
		if (node_color[start_node] == NODE_WHITE){
			size_t timer = 0;
			std::stack<dfs_stack_item> dfs_stack;
			std::vector<size_t> time_in(node_quantity_, 0);
			std::vector<size_t> min_time_in(node_quantity_, 0);
			/*here we store minimal in-time for all the special nodes that can be reached.
			Special are nodes that are the ends of reverse edges whose start node is a child of current_node.
			*/
			dfs_stack.push(dfs_stack_item(start_node, 0));
			node_t previous_visited_node;
			while (!dfs_stack.empty()){
				node_t current_node = dfs_stack.top().node;
				if (node_color[current_node] == NODE_WHITE){
					//process this node for the first time
					min_time_in[current_node] = time_in[current_node] = ++timer;
					node_color[current_node] = NODE_GREY;
					/*if (mode == COMPONENT_MODE){
						node_stack.push(current_node);
					}*/
				}
				else {
					//node_color[current_node] is NODE_GREY => returned to the node from its child
					min_time_in[current_node] = std::min(min_time_in[current_node], min_time_in[previous_visited_node]);
					//check whether edge [current_node, previous_visited_node] is a bridge
					if (time_in[current_node] < min_time_in[previous_visited_node]){
						bridges.push_back(graph_[current_node][dfs_stack.top().next_neighbour_index-1].edge_index);
						/*if (mode == COMPONENT_MODE){
							++edge_component_quantity;
							while (node_stack.top() != previous_visited_node){
								edge_component[node_stack.top()] = edge_component_quantity;
								node_stack.pop();
							}
							edge_component[node_stack.top()] = edge_component_quantity;
							node_stack.pop();
						}*/
					}
				}

				size_t neighbour_index = dfs_stack.top().next_neighbour_index;

				while (neighbour_index < graph_[current_node].size()){
					node_t neighbour = graph_[current_node][neighbour_index].node;
					if (node_color[neighbour] != NODE_WHITE){
						node_t parent = get_parent(dfs_stack);
						//tried to return to the parent
						if (node_color[neighbour] == NODE_GREY && (neighbour != parent || parent_touched[current_node])){
							min_time_in[current_node] = std::min(min_time_in[current_node], time_in[neighbour]);
						}
						parent_touched[current_node] = parent_touched[current_node] || neighbour == parent;
						++neighbour_index;
					}
					else {
						break;
					}
				}
				if (neighbour_index >= graph_[current_node].size()){
					node_color[current_node] = NODE_BLACK;
					previous_visited_node = current_node;
					dfs_stack.pop();
				} else {
					dfs_stack.pop();
					dfs_stack.push(dfs_stack_item(current_node, neighbour_index+1));
					dfs_stack.push(dfs_stack_item(graph_[current_node][neighbour_index].node, 0));
				}
			}
		}
	}
}

const std::string INPUT_FILE  = "bridges.in";
const std::string OUTPUT_FILE = "bridges.out";

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
		edges.push_back(edge_t(node1-1, node2-1, edge_index));
	}
	fin.close();
	undirected_graph graph(node_quantity, edge_quantity, edges);
	std::vector<size_t> bridges;
	std::vector<size_t> edge_component;
	graph.bridges(bridges, COMPONENT_MODE, edge_component);

	/*std::vector<size_t> cut_vertices;
	std::vector<size_t> node_component;
	graph.cut_vertices(cut_vertices, COMPONENT_MODE, node_component);
*/
	std::sort(bridges.begin(), bridges.end());
	std::ofstream fout;
	fout.open(OUTPUT_FILE.c_str(), std::ofstream::out);
	fout << bridges.size() << "\n";
	for (size_t bridge_index = 0; bridge_index < bridges.size(); ++bridge_index){
		//fout << bridges[bridge_index] << ") " << edges[bridges[bridge_index]].first << "-" << edges[bridges[bridge_index]].second << "\n";
		fout << bridges[bridge_index]+1 << " ";
	}
/*	for (node_t node = 0; node < node_quantity; ++node){
		fout << edge_component[node] << " ";
	}
	fout << "\n\n";

	for (size_t cut_vertex_index = 0; cut_vertex_index < cut_vertices.size(); ++cut_vertex_index){
		fout << cut_vertices[cut_vertex_index] << "\n";
	}
	for (size_t edge_index = 0; edge_index < edge_quantity; ++edge_index){
		fout << node_component[edge_index] << " ";
	}*/
	fout.close();
	return 0;
}
