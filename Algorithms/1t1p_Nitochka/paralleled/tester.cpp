#include "tester.h"

std::string niceTestResultOutput(int number, double time, int points, double answer) {
	std::stringstream out;
	out << "Test #" << number << ".\n";
	out << "\tPoints: " << points << '\n';
	out << "\tTime: " << time << '\n';
	out << "\tAnswer: " << answer << '\n';
	out << kTestDelimiter;
	return out.str();
}

TimeTester::TimeTester()
	: test_number_(0) {}

int TimeTester::test_number() const {
	return test_number_;
}

double TimeTester::last_test_time() const {
	return last_test_time_;
}

int TimeTester::last_test_point_quantity() const {
	return last_test_point_quantity_;
}

double TimeTester::last_test_answer() const {
	return last_test_answer_;
}

void TimeTester::generateTimeTest(PointSet& point_set) const {
	int point_quantity = rand() % (kTimeMaxPointQuantity - 2) + 3;
	point_set.points.reserve(point_quantity);
	for (int point_index = 0; point_index < point_quantity; ++point_index) {
		point_set.points.push_back(Point(rand() % kMaxCoord, rand() % kMaxCoord));
	}
}

std::string TimeTester::testResults() const {
	return niceTestResultOutput(test_number_, last_test_time_,
								last_test_point_quantity_, last_test_answer_);
}

void TimeTester::timeTest() {
	++test_number_;
	PointSet point_set;
	generateTimeTest(point_set);
	last_test_point_quantity_ = point_set.points.size();

	double start_timer = omp_get_wtime();
	last_test_answer_ = minAreaOfHullWithNodeRemoved(point_set);
	double stop_timer = omp_get_wtime();
	last_test_time_ = stop_timer - start_timer;
}

void TimeTester::timeTestLauncher(size_t test_quantity) {
	for (size_t test_index = 0; test_index < test_quantity; ++test_index) {
		std::cerr << test_number_ << " ";
		timeTest();
	}
}

void TimeTester::timeTestLauncher() {
	timeTestLauncher(rand() % kTimeMaxTestQuantity);
}

