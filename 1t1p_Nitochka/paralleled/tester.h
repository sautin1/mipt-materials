#ifndef TESTER_H
#define TESTER_H

#include <cstdlib>
#include <ctime>
#include <iostream>
#include "convex_hull.h"

const int kTimeMaxTestQuantity = 5;
const int kTimeMaxPointQuantity = 1e5;
const int kMaxCoord = 1e9;

void generateTimeTest(PointSet& point_set);
void timeTest();
void timeTestLauncher(size_t test_quantity);
void timeTestLauncher();

#endif // TESTER_H
