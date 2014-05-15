#include <iostream>
#include <fstream>
#include <vector>
#include "test_system.h"

const std::string INPUT_FILE  = "cruscal_TSP.in";
const std::string OUTPUT_FILE = "cruscal_TSP.out";

int main()
{
	//Autotests
	testTSPSolver();

	/*
	//Manual tests
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
	fout << result << "\n";
	for (size_t city_index = 0; city_index < path.size(); ++city_index){
		fout << path[city_index] << " ";
	}
	fout.close();
	*/
}
