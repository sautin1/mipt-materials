#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cstdio>
#include <cassert>
#include <chrono>

void printArray(int* array, int array_size) {
	std::cout << "[ ";
	for (int i = 0; i < array_size-1; ++i) {
		std::cout << array[i] << ", ";
	}
	std::cout << array[array_size-1];
	std::cout << " ]\n";
}

class TimeMeasurer {
public:
	void start() {
		start_ = std::chrono::high_resolution_clock::now();
	}

	long long stop() {
		auto stop = std::chrono::high_resolution_clock::now();
		auto delta = std::chrono::duration_cast<std::chrono::nanoseconds>(stop - start_).count();
		assert((sizeof delta) == 8);
		return delta;
	}
private:
	std::chrono::high_resolution_clock::time_point start_;
};

class BinSearchSlow {
public:
	BinSearchSlow(int* array, int array_size)
		: array_(array), array_size_(array_size) {}

	void init() {}
	int search(int left, int right, int search_value) {
		while (left < right) {
			int middle = (left + right) / 2;
			if (array_[middle] <= search_value) {
				left = middle + 1;
			} else {
				right = middle;
			}
		}
		return left;
	}

private:
	int* array_;
	int array_size_;
};

class BinSearchFast {
public:
	BinSearchFast(int* array, int array_size)
		: array_(array), array_size_(array_size) {
		jump_array_size_ = array_size / BIN_SEARCH_FAST_JUMP_SIZE;
		jump_array_ = new int[jump_array_size_];
		int jump_index = 0;
		for (int i = 0; i < jump_array_size_; ++i) {
			jump_array_[i] = array[jump_index];
			jump_index += BIN_SEARCH_FAST_JUMP_SIZE;
		}
	}

	void init() {}

	int search(int search_value) {
		BinSearchSlow bin_search_slow_jump(jump_array_, jump_array_size_);
		int right_search_index = bin_search_slow_jump.search(0, jump_array_size_, search_value);
		right_search_index = std::min(array_size_, right_search_index * BIN_SEARCH_FAST_JUMP_SIZE);
		BinSearchSlow bin_search_slow(array_, array_size_);
		int left_search_index = std::max(0, right_search_index - BIN_SEARCH_FAST_JUMP_SIZE);
		return bin_search_slow.search(left_search_index, right_search_index, search_value);
	}

	~BinSearchFast() {
		delete [] jump_array_;
	}

private:
	const int BIN_SEARCH_FAST_JUMP_SIZE = 1e3;

	int* array_;
	int* jump_array_;
	int array_size_;
	int jump_array_size_;
};

const int RANDOM_RANGE_SORTED = 1e1;

void randomFillSorted(int* array, int array_size) {
	for (int i = 0; i < array_size; ++i) {
		array[i] = rand() % RANDOM_RANGE_SORTED;
		if (i) {
			array[i] += array[i-1];
		}
	}
}

const int ARRAY_SIZE = 1e8;
const int RANDOM_RANGE_UNSORTED = ARRAY_SIZE * RANDOM_RANGE_SORTED / 2;

void randomFill(int* array, int array_size) {
	for (int i = 0; i < array_size; ++i) {
		array[i] = rand() % RANDOM_RANGE_UNSORTED;
	}
}

const int TEST_QUANTITY = 1e5;

long long test_bin_search_slow(int* array, int* queries, int* answers) {
	TimeMeasurer timer;
	BinSearchSlow bin_search_slow(array, ARRAY_SIZE);
	bin_search_slow.init();
	timer.start();
	for (int test_index = 0; test_index < TEST_QUANTITY; ++test_index) {
		answers[test_index] = bin_search_slow.search(0, ARRAY_SIZE, queries[test_index]);
	}
	return timer.stop();
}

long long test_bin_search_fast(int* array, int* queries, int* answers) {
	TimeMeasurer timer;
	BinSearchFast bin_search_fast(array, ARRAY_SIZE);
	bin_search_fast.init();
	timer.start();
	for (int test_index = 0; test_index < TEST_QUANTITY; ++test_index) {
		answers[test_index] = bin_search_fast.search(queries[test_index]);
	}
	return timer.stop();
}


int main() {
	srand(time(NULL));
	// create primary array of ints
	int* array = new int[ARRAY_SIZE];
	randomFillSorted(array, ARRAY_SIZE);
	std::cout << "# Array size: " << ARRAY_SIZE << "\n";

	// generate tests
	int* queries = new int[TEST_QUANTITY];
	randomFill(queries, TEST_QUANTITY);
	std::cout << "# Queries: " << TEST_QUANTITY << "\n";

	// test slow binary search
	std::cout << "# Slow bin_search: ";
	int* answers_slow = new int[TEST_QUANTITY];
	std::cout << test_bin_search_slow(array, queries, answers_slow) << " ns \n";

	// warm up fast binary search
	int* answers_fast = new int[TEST_QUANTITY];
	test_bin_search_fast(array, queries, answers_fast);

	// test fast binary search
	std::cout << "# Fast bin_search: ";
	std::cout << test_bin_search_fast(array, queries, answers_fast) << " ns \n";

	// check that test results are equal
	for (int test_index = 0; test_index < TEST_QUANTITY; ++test_index) {
		assert(answers_fast[test_index] == answers_slow[test_index]);
	}

	delete [] array;
	delete [] answers_slow;
	delete [] answers_fast;
	return 0;
}

