#include <iostream>
#include <fstream>
#include <string>
#include <stdexcept>

#include <vector>
#include <algorithm>

class component_set
{
	std::vector<ssize_t> parent_;
	std::vector<size_t> component_rank_;
	std::vector<size_t> component_weight_;
	size_t node_quantity_;
	size_t size_;
public:
	component_set(size_t node_quantity)
	{
		component_rank_.assign(node_quantity, 1);
		parent_.assign(node_quantity, -1);
		component_weight_.assign(node_quantity, 0);
		node_quantity_ = node_quantity;
		size_ = node_quantity;
	}

	size_t find_set(const size_t node)
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

	bool unite(size_t node1, size_t node2, size_t weight)
	{
		size_t root1 = find_set(node1);
		size_t root2 = find_set(node2);
		if (root1 == root2){
			component_weight_[root1] += weight;
			return false;
		}
		if (component_rank_[root1] > component_rank_[root2]){
			std::swap<size_t>(node1, node2);
			std::swap<size_t>(root1, root2);
		}

		//Add component1 to component2 root
		component_weight_[root2] += weight + component_weight_[root1];
		parent_[root1] = root2;
		if (component_rank_[root1] == component_rank_[root2]){
			++component_rank_[root2];
		}
		--size_;
		return true;
	}

	size_t size() const
	{
		return size_;
	}

	size_t comp_weight(size_t comp_id)
	{
		return component_weight_[find_set(comp_id)];
	}
};

const std::string INPUT_FILE  = "input.txt";
const std::string OUTPUT_FILE = "output.txt";

int main()
{
	std::ifstream fin;
	fin.open(INPUT_FILE.c_str(), std::ifstream::in);
	if (!fin.good()){
		throw std::runtime_error(std::string("Cannot open INPUT FILE: ") + INPUT_FILE);
	}
	std::ofstream fout;
	fout.open(OUTPUT_FILE.c_str(), std::ofstream::out);
	if (!fout.good()){
		throw std::runtime_error(std::string("Cannot open OUTPUT FILE: ") + OUTPUT_FILE);
	}
	size_t node_quantity;
	size_t query_quantity;
	fin >> node_quantity >> query_quantity;
	component_set ccs(node_quantity); //ccs - Connected Component Set
	size_t result = 0;
	for (size_t query_index = 0; query_index < query_quantity; ++query_index){
		size_t mode;
		fin >> mode;
		if (mode == 1){
			size_t node1;
			size_t node2;
			size_t weight;
			fin >> node1 >> node2 >> weight;
			ccs.unite(node1-1, node2-1, weight);
		} else {
			size_t node;
			fin >> node;
			fout << ccs.comp_weight(node-1) << "\n";
		}
	}
	fin.close();
	fout.close();
	return 0;
}