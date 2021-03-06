#ifndef TEST_SYSTEM_H
#define TEST_SYSTEM_H

#include "binomial_heap.h"
#include <vector>
#include <iostream>
#include <cstdlib>
#include <ctime>
#include <algorithm>

typedef std::shared_ptr< binomial_tree< int, std::less<int> > > binomial_tree_ptr;

const size_t TEST_Q = 50;
const size_t MAX_MELD_Q = 500;
const size_t MAX_HEAP_SIZE = 100000;

void binomial_heap_tester();
void binomial_heap_test_caller();
void binomial_heap_test_caller(size_t test_quantity);

#endif // TEST_SYSTEM_H
