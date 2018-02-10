#ifndef TESTER_H
#define TESTER_H

#include <cstdlib>
#include <ctime>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include "convex_hull.h"


const std::string kTestDelimiter = "__________\n";

std::string niceTestResultOutput(int number, double time, int points, double answer);

class TimeTester {
public:
	TimeTester();
	void timeTestLauncher(size_t test_quantity);
	void timeTestLauncher();
	std::string testResults() const;

	int test_number() const;
	double last_test_time() const;
	int last_test_point_quantity() const;
	double last_test_answer() const;
private:
	const int kTimeMaxTestQuantity = 5;
	const int kTimeMaxPointQuantity = 2e5;
	const int kMaxCoord = 1e9;

	int test_number_;
	double last_test_time_;
	int last_test_point_quantity_;
	double last_test_answer_;

	void generateTimeTest(PointSet& point_set) const;
	void timeTest();
};

#endif // TESTER_H
