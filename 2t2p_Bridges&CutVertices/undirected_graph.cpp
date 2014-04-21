#include "undirected_graph.h"

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

ssize_t undirected_graph::get_parent(std::stack<dfs_stack_item>& dfs_stack) const
{
	//returns the parent node of dfs_stack.top().node (.node field of the pretop item in dfs_stack)
	ssize_t parent;
	if (dfs_stack.size() > 1){
		dfs_stack_item top_item = dfs_stack.top();
		dfs_stack.pop();
		parent = dfs_stack.top().node;
		dfs_stack.push(top_item);
	} else {
		parent = -1;
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
	if (mode == COMPONENT_MODE){
		edge_component.assign(node_quantity_, 0);
	}

	for (node_t start_node = 0; start_node < node_quantity_; ++start_node){
		if (node_color[start_node] == NODE_WHITE){
			size_t timer = 0;
			std::stack<dfs_stack_item> dfs_stack;
			std::vector<size_t> time_in(node_quantity_, 0);
			std::vector<size_t> min_time_in(node_quantity_, 0);
			/*here we store minimal in-time for all the "special" nodes that can be reached.
			"Special" are nodes that are the ends of reverse edges whose start node is a child of current_node.
			*/
			dfs_stack.push(dfs_stack_item(start_node, 0));
			node_t previous_visited_node;
			while (!dfs_stack.empty()){
				node_t current_node = dfs_stack.top().node;
				if (node_color[current_node] == NODE_WHITE){
					//process this node for the first time
					min_time_in[current_node] = time_in[current_node] = ++timer;
					node_color[current_node] = NODE_GREY;
				}
				else {
					//node_color[current_node] is NODE_GREY => returned to the node from its child
					min_time_in[current_node] = std::min(min_time_in[current_node], min_time_in[previous_visited_node]);
					//check whether edge [current_node, previous_visited_node] is a bridge
					if (time_in[current_node] < min_time_in[previous_visited_node]){
						bridges.push_back(graph_[current_node][dfs_stack.top().next_neighbour_index-1].edge_index);
						if (mode == COMPONENT_MODE){
							++edge_component_quantity;
							while (node_stack.top() != current_node){
								edge_component[node_stack.top()] = edge_component_quantity - 1;
								node_stack.pop();
							}
						}
					}
				}
				if (mode == COMPONENT_MODE){
					node_stack.push(current_node);
				}

				size_t neighbour_index = dfs_stack.top().next_neighbour_index;

				while (neighbour_index < graph_[current_node].size()){
					node_t neighbour = graph_[current_node][neighbour_index].node;
					if (node_color[neighbour] != NODE_WHITE){
						ssize_t parent = get_parent(dfs_stack);
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
			if (mode == COMPONENT_MODE){
				++edge_component_quantity;
				while (!node_stack.empty()){
					edge_component[node_stack.top()] = edge_component_quantity - 1;
					node_stack.pop();
				}
			}
		}
	}
}


void undirected_graph::cut_vertices(std::vector<node_t>& cut_vertices, size_t mode, std::vector<ssize_t>& node_component) const
{
	cut_vertices.clear();
	std::vector<size_t> node_color(node_quantity_, NODE_WHITE);
	std::vector<bool> iscut(node_quantity_, false); //used to avoid repeats in cut_vertices
	std::vector<bool> parent_touched(node_quantity_, false); //is used only to process multiple edges
	std::stack<size_t> edge_stack; //COMPONENT MODE
	std::vector<ssize_t> previous_edge; //COMPONENT MODE
	size_t node_component_quantity = 0; //COMPONENT MODE
	if (mode == COMPONENT_MODE){
		node_component.assign(edge_quantity_, -1);
		previous_edge.assign(node_quantity_, -1);
	}

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
			size_t root_neighbours = 0; //used to check if start_node is a cut-vertex
			while (!dfs_stack.empty()){
				node_t current_node = dfs_stack.top().node;
				if (node_color[current_node] == NODE_WHITE){
					//process this node for the first time
					min_time_in[current_node] = time_in[current_node] = ++timer;
					node_color[current_node] = NODE_GREY;
				}
				else {
					//node_color[current_node] is NODE_GREY => returned to the node from its child
					if (mode == COMPONENT_MODE){
						edge_stack.push(graph_[current_node][dfs_stack.top().next_neighbour_index-1].edge_index);
					}
					if (current_node == start_node) {
						++root_neighbours;
						if (root_neighbours > 1){
							if (!iscut[start_node]){
								cut_vertices.push_back(start_node);
								iscut[start_node] = true;
							}
							if (mode == COMPONENT_MODE){
								++node_component_quantity;
								while (!edge_stack.empty() && edge_stack.top() != previous_edge[current_node]){
									node_component[edge_stack.top()] = node_component_quantity - 1;
									edge_stack.pop();
								}
							}
						} else{
							if (mode == COMPONENT_MODE){
								previous_edge[current_node] = graph_[current_node][dfs_stack.top().next_neighbour_index-1].edge_index;
							}
						}
					}
					else {
						min_time_in[current_node] = std::min(min_time_in[current_node], min_time_in[previous_visited_node]);
						//check whether current_node is a cut-vertex
						if (time_in[current_node] <= min_time_in[previous_visited_node]){
							if (!iscut[current_node]){
								cut_vertices.push_back(current_node);
								iscut[current_node] = true;
							}
							if (mode == COMPONENT_MODE){
								++node_component_quantity;
								while (!edge_stack.empty() && edge_stack.top() != previous_edge[current_node]){
									node_component[edge_stack.top()] = node_component_quantity - 1;
									edge_stack.pop();
								}
							}
						} else{
							if (mode == COMPONENT_MODE){
								previous_edge[current_node] = graph_[current_node][dfs_stack.top().next_neighbour_index-1].edge_index;
							}
						}
					}
				}

				size_t neighbour_index = dfs_stack.top().next_neighbour_index;

				while (neighbour_index < graph_[current_node].size()){
					node_t neighbour = graph_[current_node][neighbour_index].node;

					if (node_color[neighbour] != NODE_WHITE){
						ssize_t parent = get_parent(dfs_stack);
						//tried to return to the parent
						if (node_color[neighbour] == NODE_GREY && (neighbour != parent || parent_touched[current_node])){
							min_time_in[current_node] = std::min(min_time_in[current_node], time_in[neighbour]);
							if (mode == COMPONENT_MODE){
								previous_edge[current_node] = graph_[current_node][neighbour_index].edge_index;
								edge_stack.push(graph_[current_node][neighbour_index].edge_index);
							}
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
					previous_visited_node = dfs_stack.top().node;
					dfs_stack.pop();
				} else {
					dfs_stack.pop();
					dfs_stack.push(dfs_stack_item(current_node, neighbour_index+1));
					dfs_stack.push(dfs_stack_item(graph_[current_node][neighbour_index].node, 0));
					if (mode == COMPONENT_MODE){
						previous_edge[graph_[current_node][neighbour_index].node] = graph_[current_node][neighbour_index].edge_index;
						edge_stack.push(graph_[current_node][neighbour_index].edge_index);
					}
				}
			}
			if (mode == COMPONENT_MODE && !edge_stack.empty()){
				++node_component_quantity;
				while (!edge_stack.empty()){
					node_component[edge_stack.top()] = node_component_quantity - 1;
					edge_stack.pop();
				}
			}
		}
	}
}

size_t undirected_graph::linked_components(ssize_t excluded_node) const
{
	/*returns the number of linked components in the graph, where excluded_node is excluded*/
	std::vector<size_t> node_color(node_quantity_, NODE_WHITE);
	if (excluded_node > -1){
		node_color[excluded_node] = NODE_BLACK;
	}
	size_t component_quantity = 0;
	for (node_t start_node = 0; start_node < node_quantity_; ++start_node){
		if (node_color[start_node] == NODE_WHITE){
			++component_quantity;
			std::stack<dfs_stack_item> dfs_stack;
			dfs_stack.push(dfs_stack_item(start_node, 0));
			while (!dfs_stack.empty()){
				node_t current_node = dfs_stack.top().node;
				node_color[current_node] = NODE_GREY;
				size_t neighbour_index = dfs_stack.top().next_neighbour_index;

				while (neighbour_index < graph_[current_node].size() && node_color[graph_[current_node][neighbour_index].node] != NODE_WHITE){
					++neighbour_index;
				}
				if (neighbour_index >= graph_[current_node].size()){
					node_color[current_node] = NODE_BLACK;
					dfs_stack.pop();
				} else {
					dfs_stack.pop();
					dfs_stack.push(dfs_stack_item(current_node, neighbour_index+1));
					dfs_stack.push(dfs_stack_item(graph_[current_node][neighbour_index].node, 0));
				}
			}
		}
	}
	return component_quantity;
}
