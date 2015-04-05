#include <fstream>
#include <iostream>
#include <vector>
#include "geometry.h"

int main() {
	std::ios_base::sync_with_stdio(false);
	std::vector<Circle> trees;
	std::ifstream fin("INPUT.TXT");
	size_t tree_quantity;
	fin >> tree_quantity;
	trees.reserve(tree_quantity);
	for (size_t tree_index = 0; tree_index < tree_quantity; ++tree_index) {
		double x, y, radius;
		fin >> x >> y >> radius;
		trees.push_back(Circle(Point(x, y), radius + kEpsilon));
	}
	fin.close();
	bool found_road = false;
	for (size_t i = 0; i < tree_quantity-1; ++i) {
		for (size_t j = i + 1; j < tree_quantity; ++j) {
			std::vector<Line> tangents = getTangents(trees[i], trees[j]);
			for (int tangent_index = 1; tangent_index <= 2; ++tangent_index) {
				found_road = true;
				for (size_t k = 0; k < tree_quantity; ++k) {
					if (k == i || k == j) {
						continue;
					}
					if (intersects(tangents[tangent_index], trees[k])) {
						found_road = false;
						break;
					}
				}
				if (found_road) {
					break;
				}
			}
			if (found_road) {
				break;
			}
		}
		if (found_road) {
			break;
		}
	}

	std::ofstream fout("OUTPUT.TXT");
	if (found_road) {
		fout << "NO\n";
	} else {
		fout << "YES\n";
	}
	fout.close();

	return 0;
}
