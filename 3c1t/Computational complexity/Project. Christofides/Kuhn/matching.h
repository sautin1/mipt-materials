#pragma once
#include <vector>

#include "graph.h"

using Matching = std::vector<Edge>;
using NodeMatches = std::vector<int>;

bool augmentingPath(const Graph& graph, NodeMatches& matching,
                    std::vector<bool>& visited, int node);
Matching nodeMatchesToMatching(const NodeMatches& matches);
Matching maximalMatchingBipartite(const Graph& graph);
