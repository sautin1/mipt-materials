#pragma once
#include <stdexcept>
#include <ctime>
#include <iostream>
#include "tsp.h"

const size_t TEST_Q = 1000;
const size_t CENTRES_Q = 1000;
const size_t CITIES_Q = 500;
const size_t TOTAL_CITIES_Q = CENTRES_Q * CITIES_Q; //(V^2)logV
const size_t SIGMA1 = 100;
const size_t SIGMA2 = 10;

city_t create_random_city(size_t sigma);
void tester();
void testTSPSolver(size_t test_quantity);
void testTSPSolver();
