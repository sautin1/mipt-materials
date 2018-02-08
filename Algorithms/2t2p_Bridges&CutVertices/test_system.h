#pragma once
#include <ctime>
#include <cstdlib>
#include <set>
#include "undirected_graph.h"

/*bridge_tester complexity is O(E^2 + BlogB + E*V), where B - number of bridges*/
const size_t MAX_NODE_QUANTITY = 700/*10*/;
const size_t MIN_NODE_QUANTITY = 100/*3*/;
const size_t MAX_EDGE_QUANTITY = 700/*12*/; //if you make it bigger, the number of bridges will reduce
const size_t TEST_QUANTITY = 1000;

void test_cutvertices();
void test_cutvertices(size_t test_quantity);
void test_bridges();
void test_bridges(size_t test_quantity);
void cutvertices_tester();
void bridge_tester();
