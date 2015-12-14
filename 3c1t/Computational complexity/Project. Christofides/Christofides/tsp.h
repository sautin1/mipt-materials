#pragma once
#include <algorithm>
#include <vector>
#include "graph.h"

/**
 * Approximation algorithm for solving metric TSP
 * with approximation ratio = 2.
 * Based on MST construction.
 * TSP is metric if graph is complete and edge costs satisfy the triangle inequality.
 */
Graph tspApproximationMST(const CompleteGraph& graph);



/**
 * Christofides approximation algorithm for solving metric TSP
 * with approximation ratio = 3/2.
 * TSP is metric if graph is complete and edge costs satisfy the triangle inequality.
 */
Graph tspApproximationChristofides(const CompleteGraph& graph);
