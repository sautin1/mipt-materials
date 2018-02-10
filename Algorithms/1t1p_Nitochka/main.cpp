#include <fstream>
#include <iostream>

#include "convex_hull.h"

int main() {
	std::ifstream fin("test5.in");
	int point_quantity;
	fin >> point_quantity;
	PointSet point_set;
	point_set.points.reserve(point_quantity);
	for (int nail_index = 0; nail_index < point_quantity; ++nail_index) {
		int x, y;
		fin >> x >> y;
		point_set.points.push_back(Point(x, y));
	}
	fin.close();

	double result_area = minAreaOfHullWithNodeRemoved(point_set);
	std::cout << result_area << '\n';

	return 0;
}

