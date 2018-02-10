#include <fstream>
#include <iostream>
#include "convex_hull.h"

int main() {
	std::ifstream fin("test.in");
	int nail_quantity;
	fin/*std::cin*/ >> nail_quantity;
	PointSet pointSet;
	pointSet.points.reserve(nail_quantity);
	for (int nail_index = 0; nail_index < nail_quantity; ++nail_index) {
		int x, y;
		fin/*std::cin*/ >> x >> y;
		pointSet.points.push_back(Point(x, y));
	}
	fin.close();

	ConvexHull convex_hull(pointSet, false);
	std::vector<int> hull_indices = convex_hull.getHullNodesIndices();
	for (size_t indices_index = 0; indices_index < hull_indices.size(); ++indices_index) {
		std::cout << hull_indices[indices_index] << '\n';
	}
	Polygon hull_polygon = convex_hull.getHullPolygon();
	std::cout << "area: " << hull_polygon.area() << '\n';
	std::cout << "perimeter: " << hull_polygon.perimeter() << '\n';

	return 0;
}

