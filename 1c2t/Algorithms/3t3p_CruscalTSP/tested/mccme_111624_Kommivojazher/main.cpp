#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <stdexcept>
#include <limits>
#include <stack>
#include <algorithm>
//_______________________________________________________________
class dsu
{
	size_t size_; //number of components
	std::vector<ssize_t> parent_;
	std::vector<size_t> set_rank_;
public:
	dsu(size_t size);
	size_t find_set(const size_t node);
	bool unite(size_t node1, size_t node2);
	size_t size() const;
};

//_______________________________________________________________

dsu::dsu(size_t size)
	:size_(size), parent_(size, -1), set_rank_(size, 1)
{}

size_t dsu::find_set(const size_t node)
{
	size_t root;
	if (parent_[node] == -1){
		root = node;
	}
	else {
		root = find_set(parent_[node]);
		parent_[node] = root;
	}
	return root;
}

bool dsu::unite(size_t node1, size_t node2)
{
	size_t root1 = find_set(node1);
	size_t root2 = find_set(node2);
	if (root1 == root2){
		return false;
	}
	if (set_rank_[root1] > set_rank_[root2]){
		std::swap<size_t>(node1, node2);
		std::swap<size_t>(root1, root2);
	}

	//Add component1 to component2 root
	parent_[root1] = root2;
	if (set_rank_[root1] == set_rank_[root2]){
		++set_rank_[root2];
	}
	--size_;
	return true;
}

size_t dsu::size() const
{
	return size_;
}

//_______________________________________________________________
typedef size_t node_t;

//Weighted graph
typedef int64_t weight_t;
struct edge_t
{
	edge_t(node_t first_, node_t second_, weight_t weight_): first(first_), second(second_), weight(weight_){}
	bool operator<(const edge_t& edge) const{
		return (weight < edge.weight);
	}
	node_t first;
	node_t second;
	weight_t weight;
};
const weight_t INF = (std::numeric_limits<weight_t>().max() / 2) - 1;

class weighted_graph
{
	std::vector< edge_t > edges_;
	size_t node_quantity_;
public:
	weighted_graph();
	weighted_graph(size_t node_quantity, const std::vector<edge_t>& edges);
	weighted_graph(size_t node_quantity, const std::vector< std::vector<weight_t> >& matrix);
	size_t size() const;
	void MST(std::vector<edge_t>& tree_edges); //Cruscal
	void TSP_path(node_t start_node, std::vector<node_t>& path);
};

//Undirected graph

struct dfs_stack_item{
	dfs_stack_item(node_t new_node, size_t neighbour): node(new_node), next_neighbour_index(neighbour){}
	node_t node;
	size_t next_neighbour_index;
};

class undirected_graph
{
	std::vector< std::vector<node_t> > graph_;
	size_t node_quantity_;
public:
	undirected_graph(size_t node_quantity, const std::vector<edge_t>& edges);
	void dfs_tin(node_t start_node, std::vector<node_t>& tin) const;
	size_t size() const;
};
//_______________________________________________________________
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
	if (edges_.size() > 5000000){
	   std::sort(edges_.begin(), edges_.begin()+(5000000-1), std::less<edge_t>());
	} else {
	   std::sort(edges_.begin(), edges_.end(), std::less<edge_t>());
	}
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
	//path.push_back(path.front());
}

//Weighted graph
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

//_______________________________________________________________

struct city_t{
	city_t(const weight_t x_, const weight_t y_): x(x_), y(y_){}
	weight_t x, y;
};

weight_t euclidean_distance(const city_t& city1, const city_t& city2);
weight_t solveTSP(size_t city_quantity, const std::vector<city_t>& cities, std::vector<node_t>& path);

weight_t euclidean_distance(const city_t& city1, const city_t& city2)
{
	return (city1.x - city2.x)*(city1.x - city2.x) + (city1.y - city2.y)*(city1.y - city2.y);
}

weight_t solveTSP(size_t city_quantity, const std::vector<city_t>& cities, std::vector<node_t>& path)
{
	std::vector< std::vector<weight_t> > matrix(city_quantity, std::vector<weight_t>(city_quantity, INF));
	for (size_t city1_index = 0; city1_index < city_quantity; ++city1_index){
		for (size_t city2_index = city1_index; city2_index < city_quantity; ++city2_index){
			matrix[city1_index][city2_index] = euclidean_distance(cities[city1_index], cities[city2_index]);
			matrix[city2_index][city1_index] = matrix[city1_index][city2_index];
		}
	}
	weighted_graph graph(city_quantity, matrix);
	graph.TSP_path(0, path);
	weight_t path_length = 0;
	for (size_t city_index = 1; city_index < path.size(); ++city_index){
		path_length += matrix[path[city_index-1]][path[city_index]];
	}
	return path_length;
}

const std::string INPUT_FILE  = "input.txt";
const std::string OUTPUT_FILE = "output.txt";

int main()
{
	std::ifstream fin(INPUT_FILE.c_str(), std::ifstream::in);
	if (!fin.good()){
		throw std::runtime_error(std::string("Cannot open input file ") + INPUT_FILE);
	}
	size_t city_quantity;
	std::vector<city_t> cities;
	fin >> city_quantity;
	for (size_t city_index = 0; city_index < city_quantity; ++city_index){
		weight_t x, y;
		fin >> x >> y;
		cities.push_back(city_t(x, y));
	}
	fin.close();

	std::vector<node_t> path;
	weight_t result = solveTSP(city_quantity, cities, path);

	std::ofstream fout(OUTPUT_FILE.c_str(), std::ofstream::out);
	if (!fout.good()){
		throw std::runtime_error(std::string("Cannot open/create output file ") + OUTPUT_FILE);
	}
	//fout << result << "\n";
	for (size_t city_index = 0; city_index < path.size(); ++city_index){
		fout << path[city_index]+1 << " ";
	}
	fout.close();
}
