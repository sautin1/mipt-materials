#include "weighted_graph.h"

weighted_graph::weighted_graph(){}

weighted_graph::weighted_graph(size_t node_quantity, const std::vector<edge_t>& edges)
	:edges_(edges), node_quantity_(node_quantity)
{}

weighted_graph::weighted_graph(size_t node_quantity, const std::vector< std::vector<weight_t> >& matrix)
	:edges_(), node_quantity_(node_quantity)
{
	for (node_t node1 = 0; node1 < node_quantity; ++node1){
		for (node_t node2 = 0; node2 < node_quantity; ++node2){
			if (matrix[node1][node2] < INF && node1 != node2){
				edges_.push_back(edge_t(node1, node2, matrix[node1][node2]));
			}
		}
	}
}

size_t weighted_graph::size() const
{
	return node_quantity_;
}

void weighted_graph::MST(std::vector<edge_t>& tree_edges)
{
	std::sort(edges_.begin(), edges_.end(), std::less<edge_t>());
	dsu mst_dsu(node_quantity_);
	for (size_t edge_index = 0; edge_index < edges_.size(); ++edge_index){
		bool used_edge = mst_dsu.unite(edges_[edge_index].first, edges_[edge_index].second);
		if (used_edge){
			tree_edges.push_back(edges_[edge_index]);
		}
		if (mst_dsu.size() == 1){
			break;
		}
	}
}

void weighted_graph::TSP_path(node_t start_node, std::vector<node_t>& path)
{
	std::vector<edge_t> tree_edges;
	MST(tree_edges);
	undirected_graph graph(node_quantity_, tree_edges);
	graph.dfs_tin(start_node, path);
	path.push_back(path.front());
}

//Undirected graph
undirected_graph::undirected_graph(size_t node_quantity, const std::vector<edge_t>& edges)
	:graph_(node_quantity, std::vector<node_t>()), node_quantity_(node_quantity)
{
	for (std::vector<edge_t>::const_iterator edge_it = edges.begin(); edge_it != edges.end(); ++edge_it){
		graph_[edge_it->first].push_back(edge_it->second);
		if (edge_it->first != edge_it->second) {
			graph_[edge_it->second].push_back(edge_it->first);
		}
	}
}

size_t undirected_graph::size() const
{
	return node_quantity_;
}

void undirected_graph::dfs_tin(node_t start_node, std::vector<node_t>& tin) const
{
	tin.clear();
	std::vector<bool> used(node_quantity_, false);
	std::stack<dfs_stack_item> dfs_stack;
	dfs_stack.push(dfs_stack_item(start_node, 0));
	while (!dfs_stack.empty()){
		node_t current_node = dfs_stack.top().node;
		if (!used[current_node]){
			tin.push_back(current_node);
		}
		used[current_node] = true;
		size_t neighbour_index = dfs_stack.top().next_neighbour_index;

		while (neighbour_index < graph_[current_node].size() && used[graph_[current_node][neighbour_index]]){
			++neighbour_index;
		}
		if (neighbour_index >= graph_[current_node].size()){
			dfs_stack.pop();
		} else {
			dfs_stack.pop();
			dfs_stack.push(dfs_stack_item(current_node, neighbour_index+1));
			dfs_stack.push(dfs_stack_item(graph_[current_node][neighbour_index], 0));
		}
	}
}
