#include <iostream>
#include <fstream>
#include <string>
#include "component_set.h"
#include <stdexcept>

const std::string INPUT_FILE  = "ccs.in";
const std::string OUTPUT_FILE = "ccs.out";

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
	for (size_t query_index = 0; query_index < query_quantity; ++query_index){
		char query_mode;
		fin >> query_mode;
		size_t node1;
		size_t node2;
		fin >> node1 >> node2;
		if (query_mode == 'U'){
			//Unite two nodes
			ccs.unite(node1-1, node2-1);
		} else if (query_mode == 'C'){
			//Check if two nodes are connected
			size_t root1 = ccs.find_set(node1-1);
			size_t root2 = ccs.find_set(node2-1);
			if (root1 == root2){
				fout << "YES" << "\n";
			} else{
				fout << "NO" << "\n";
			}
		} else{
			throw std::runtime_error(std::string("Wrong input format!"));
		}
	}
	//fout << ccs.size();
	fin.close();
	fout.close();
	return 0;
}

