#pragma once
#include "weighted_graph.h"
#include <cmath>

struct city_t{
	city_t(){}
	city_t(const weight_t x_, const weight_t y_): x(x_), y(y_){}
	weight_t x, y;
};

weight_t euclidean_distance(const city_t& city1, const city_t& city2);
weight_t solveTSP(size_t city_quantity, const std::vector<city_t>& cities, std::vector<node_t>& path);
