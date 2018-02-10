#include <fstream>
#include <iostream>
#include "tester.h"

int main() {
	std::ofstream out("test_time_4_core.txt", std::ios_base::app);
	TimeTester tester;
	out << "# STATIC PARALLELING #\n";
	for (size_t test_index = 0; test_index < 100; ++test_index) {
		tester.timeTestLauncher(1);
		// test only function
		double time = stop_timer - start_timer;
//		std::cout << niceTestResultOutput(tester.test_number(), time,
//										  tester.last_test_point_quantity(),
//										  tester.last_test_answer());
		out << time << "\n";

		// test whole program
//		std::cout << tester.testResults();
	}
	out.close();
	std::cout << "finished\n";
	return 0;
}
