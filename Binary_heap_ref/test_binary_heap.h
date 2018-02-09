#ifndef TEST_BINARY_HEAP_H
#define TEST_BINARY_HEAP_H

#include <ctime>
#include <stdexcept>
#include <algorithm>
#include "binary_heap_ref.h"

const size_t MAX_INSERT_Q = 10000;
const size_t MAX_ERASE_Q = MAX_INSERT_Q / 3;
const size_t MAX_UPDATE_Q = MAX_INSERT_Q / 2;
const int MAX_KEY = MAX_INSERT_Q * 2;
const size_t TEST_Q = 100;

void tester();
void test_binary_heap();
void test_binary_heap(size_t test_q);

#endif // TEST_BINARY_HEAP_H
