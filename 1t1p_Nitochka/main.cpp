#include <fstream>
#include <iostream>
#include "convex_hull.h"

int main() {
	std::ifstream fin("test4.in");
	int point_quantity;
	fin/*std::cin*/ >> point_quantity;
	PointSet point_set;
	point_set.points.reserve(point_quantity);
	for (int nail_index = 0; nail_index < point_quantity; ++nail_index) {
		int x, y;
		fin/*std::cin*/ >> x >> y;
		point_set.points.push_back(Point(x, y));
	}
	fin.close();

	double result_area = minAreaOfHullWithNodeRemoved(point_set);
	std::cout << result_area << '\n';

	return 0;
}

