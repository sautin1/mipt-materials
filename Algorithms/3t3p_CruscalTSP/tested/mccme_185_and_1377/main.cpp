#include <fstream>
#include <vector>
#include <iostream>
#include <stdexcept>
#include <limits>
#include <cmath>
#include <stack>
#include <algorithm>

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

typedef size_t node_t;

typedef long weight_t;
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
	weighted_graph(size_t node_quantity, const std::vector<edge_t>& edges);
	void MST(std::vector<edge_t>& tree_edges); //Cruscal
};

weighted_graph::weighted_graph(size_t node_quantity, const std::vector<edge_t>& edges)
	:edges_(edges), node_quantity_(node_quantity)
{}

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

const std::string INPUT_FILE  = "input.txt";
const std::string OUTPUT_FILE = "output.txt";

int main()
{
	std::ifstream fin(INPUT_FILE.c_str(), std::ifstream::in);
	if (!fin.good()){
		throw std::runtime_error(std::string("Cannot open input file ") + INPUT_FILE);
	}
	size_t node_quantity, edge_quantity;
	std::vector<edge_t> edges;
	fin >> node_quantity >> edge_quantity;
	for (size_t edge_index = 0; edge_index < edge_quantity; ++edge_index){
		node_t from, to;
		weight_t weight;
		fin >> from >> to >> weight;
		edges.push_back(edge_t(from-1, to-1, weight));
	}
	fin.close();

	weighted_graph graph(node_quantity, edges);
	std::vector<edge_t> path;
	graph.MST(path);
	weight_t result = 0;
	for (size_t edge_number = 0; edge_number < path.size(); ++edge_number){
		result += path[edge_number].weight;
	}

	std::ofstream fout(OUTPUT_FILE.c_str(), std::ofstream::out);
	if (!fout.good()){
		throw std::runtime_error(std::string("Cannot open/create output file ") + OUTPUT_FILE);
	}
	fout << result << "\n";
	fout.close();
}
