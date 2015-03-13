#include "tester.h"

void generateTimeTest(PointSet& point_set) {
	int point_quantity = rand() % (kTimeMaxPointQuantity - 2) + 3;
	point_set.points.reserve(point_quantity);
	for (int point_index = 0; point_index < point_quantity; ++point_index) {
		point_set.points.push_back(Point(rand() % kMaxCoord, rand() % kMaxCoord));
	}
}

void timeTest() {
	PointSet point_set;
	generateTimeTest(point_set);
	double start_timer = omp_get_wtime();
	double answer = minAreaOfHullWithNodeRemoved(point_set);
	double stop_timer = omp_get_wtime();
	double time = stop_timer - start_timer;
	std::cout << "\tPoints: " << point_set.points.size() << '\n';
	std::cout << "\tTime: " << time << '\n';
	std::cout << "\tAnswer: " << answer << '\n';
}

void timeTestLauncher(size_t test_quantity) {
	for (size_t test_index = 0; test_index < test_quantity; ++test_index) {
		std::cout << "Test #" << test_index << ".\n";
		timeTest();
		std::cout << '\n';
	}
}

void timeTestLauncher() {
	timeTestLauncher(rand() % kTimeMaxTestQuantity);
}

