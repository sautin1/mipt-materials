#include <iostream>
#include "test_suffix_tree.h"
#include "test_find_occurrences.h"

int test(int argc, char** argv) {
	::testing::InitGoogleTest(&argc, argv);
	return RUN_ALL_TESTS();
}

int main(int argc, char** argv)
{
	int test_result = test(argc, argv);
	return test_result;
}

