#include "tsp.h"

weight_t euclidean_distance(const city_t& city1, const city_t& city2)
{
	return std::sqrt(std::pow(city1.x - city2.x, 2) + pow(city1.y - city2.y, 2));
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
