#pragma once
#include <stdexcept>
#include <ctime>
#include <iostream>
#include "weighted_graph.h"

/*Consider that the presence/absence of the negative cycle was already tested here:
http://informatics.mccme.ru/mod/statements/view3.php?id=260&chapterid=180#1
This test system only checks whether the found cycle is negative*/

const size_t TEST_Q = 100;
const size_t MAX_CURRENCY_Q = 200; //V, Algorithm: O(V^3)
const size_t MAX_CURRENCY_RATE = 1000;
const size_t MAX_CURRENCY_DELTA = 1; //max difference between matr[j][i] and 1.0/matr[i][j]

void tester();
void testArbitrage(size_t test_quantity);
void testArbitrage();
