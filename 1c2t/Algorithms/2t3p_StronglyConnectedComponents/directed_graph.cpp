#include "directed_graph.h"

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

size_t directed_graph::scc_kosaraju(std::vector<size_t>& scc_number) const
{
	//Kosaraju's algorithm
	scc_number.assign(node_quantity_, 0);
	std::vector<node_t> node_toposort;
	toposort(node_toposort);
	directed_graph transposed_graph;
	transpose(transposed_graph);
	std::vector<size_t> node_color(node_quantity_, NODE_WHITE);

	size_t scc_quantity = 0;
	for (size_t node_index = 0; node_index < node_quantity_; ++node_index){
		if (node_color[node_toposort[node_index]] == NODE_WHITE) {
			std::vector<node_t> weak_component;
			transposed_graph.dfs_nr(node_color, node_toposort[node_index], TOPOSORT_MODE, weak_component);
			++scc_quantity;
			for (size_t scc_node = 0; scc_node < weak_component.size(); ++scc_node){
				scc_number[weak_component[scc_node]] = scc_quantity - 1;
			}
		}
	}
	return scc_quantity;
}

size_t directed_graph::scc_tarjan(std::vector<size_t>& scc_number) const
{
	//Tarjan's algorithm
	scc_number.assign(node_quantity_, 0);
	size_t scc_quantity = 0;
	std::vector<size_t> node_color(node_quantity_, NODE_WHITE);

	for (node_t start_node = 0; start_node < node_quantity_; ++start_node){
		if (node_color[start_node] == NODE_WHITE){
			size_t timer = 0;
			std::stack<dfs_stack_item> dfs_stack;
			std::vector<node_t> dfs_vector;
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
					time_in[current_node] = ++timer;
					min_time_in[current_node] = time_in[current_node];
					node_color[current_node] = NODE_GREY;
					dfs_vector.push_back(current_node);
				}
				else {
					//node_color[current_node] is NODE_GREY => returned to the node from its child
					min_time_in[current_node] = std::min(min_time_in[current_node], min_time_in[previous_visited_node]);
				}

				size_t neighbour_index = dfs_stack.top().next_neighbour_index;

				while (neighbour_index < graph_[current_node].size()){
					node_t neighbour = graph_[current_node][neighbour_index];
					if (node_color[neighbour] != NODE_WHITE){
						//tried to return to the parent
						if (scc_number[neighbour] == 0){
							//do not go to other components
							min_time_in[current_node] = std::min(min_time_in[current_node], time_in[neighbour]);
						}
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
					if (time_in[current_node] == min_time_in[current_node]){
						//process component
						++scc_quantity;
						while (dfs_vector.back() != current_node){
							scc_number[dfs_vector.back()] = scc_quantity;
							dfs_vector.pop_back();
						}
						scc_number[dfs_vector.back()] = scc_quantity;
						dfs_vector.pop_back();
					}
				} else {
					dfs_stack.pop();
					dfs_stack.push(dfs_stack_item(current_node, neighbour_index+1));
					dfs_stack.push(dfs_stack_item(graph_[current_node][neighbour_index], 0));
				}
			}
		}
	}
	for (size_t scc_index = 0; scc_index < scc_number.size(); ++scc_index){
		scc_number[scc_index] = scc_quantity - scc_number[scc_index];
	}
	return scc_quantity;
}
