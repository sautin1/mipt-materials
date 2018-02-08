#include "directed_graph.h"

directed_graph::directed_graph(size_t node_quantity, const std::vector<edge_t>& edges)
	:graph_(node_quantity, std::vector<node_t>()),
	  node_quantity_(node_quantity)
{
	for (std::vector<edge_t>::const_iterator edge_it = edges.begin(); edge_it != edges.end(); ++edge_it){
		graph_[edge_it->first].push_back(edge_it->second);
	}
}

size_t directed_graph::size() const
{
	return node_quantity_;
}

size_t directed_graph::component_size_DFS(std::vector<bool>& visited, node_t node) const
{
	visited[node] = true;
	size_t result = 1;
	for (node_t node_index = 0; node_index < graph_[node].size(); ++node_index){
		if (!visited[graph_[node][node_index]]){
			result += component_size_DFS(visited, graph_[node][node_index]);
		}
	}
	return result;
}

bool directed_graph::one_weak_component() const
{
	/*return true, either if the graph is connected or
	 *if only one component has size > 1*/
	std::vector<bool> visited(node_quantity_, false);
	bool success;
	bool used = false;
	for (node_t node_index = 0; node_index < node_quantity_; ++node_index){
		if (!visited[node_index]) {
			size_t component_size = component_size_DFS(visited, node_index);
			success = ((component_size == 1) || !used);
			used = used || (component_size-1);
			if (!success){
				break;
			}
		}
	}
	return success;
}
/*
bool directed_graph::is_connected() const
{
	bool success = true;
	for (std::vector< neighbour_t >::const_iterator node_it = graph_.begin(); node_it != graph_.end(); ++node_it){
		success = ((*node_it).size() > 0);
		if (!success) {
			break;
		}
	}
	return success;
}
*/
bool directed_graph::is_eulerian() const
{
	bool success = one_weak_component();
	if (!success){
		return success;
	}
	std::vector<size_t> node_in_degree(node_quantity_, 0);
	for (node_t out_node_index = 0; out_node_index < node_quantity_; ++out_node_index){
		for (node_t in_node_index = 0; in_node_index < graph_[out_node_index].size(); ++in_node_index){
			++node_in_degree[graph_[out_node_index][in_node_index]];
		}
	}

	for (node_t node_index = 0; node_index < node_quantity_; ++node_index){
		success = (node_in_degree[node_index] == graph_[node_index].size());
		if (!success){
			break;
		}
	}
	return success;
}

bool directed_graph::eulerian_cycle(std::vector<node_t>& eul_cycle, node_t start_node) const
{
	if (!is_eulerian()){
		return false;
	}
	std::stack<node_t> path;
	std::vector<neighbour_t> graph_copy(graph_);
	path.push(start_node);
	while (!path.empty()){
		size_t current_node = path.top();
		if (!graph_copy[current_node].empty()){
			path.push(graph_copy[current_node].back());
			graph_copy[current_node].pop_back();
		}
		else {
			while (!path.empty() && graph_copy[current_node].empty()){
				eul_cycle.push_back(path.top());
				path.pop();
				if (!path.empty()){
					current_node = path.top();
				}
			}
		}
	}
	for (node_t node_index = 0; node_index < eul_cycle.size()/2; ++node_index){
		std::swap<node_t>(eul_cycle[node_index], eul_cycle[eul_cycle.size() - node_index - 1]);
	} //reverse the eulerian cycle
	//eul_cycle.pop_back(); //the last element is repeated, delete it
	return true;
}
