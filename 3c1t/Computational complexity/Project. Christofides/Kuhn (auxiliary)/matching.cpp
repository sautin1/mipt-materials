#include "matching.h"

bool augmentingPath(const Graph& graph, NodeMatches& matches,
                    std::vector<bool>& visited, int node) {
    if (visited[node]) {
        return false;
    }
    visited[node] = true;
    for (size_t i = 0; i < graph.getNeighbors(node).size(); ++i) {
        int neighbor = graph.getNeighbors(node).at(i);
        if (matches[neighbor] == -1 ||  augmentingPath(graph, matches,
                                                       visited, matches[neighbor])) {
            matches[neighbor] = node;
            matches[node]     = neighbor;
            return true;
        }
    }
    return false;
}

Matching nodeMatchesToMatching(const NodeMatches& matches) {
    Matching matching;
    std::vector<bool> used(matches.size(), false);
    for (size_t node = 0; node < matches.size(); ++node) {
        if (!used[node] && matches[node] != -1) {
            matching.push_back(Edge(node, matches[node]));
            used[node] = true;
            used[matches[node]] = true;
        }
    }
    return matching;
}

Matching maximalMatchingBipartite(const Graph& graph) {
    NodeMatches matches(graph.size(), -1);
    // create initial matching
    for (size_t node = 0; node < graph.size(); ++node) {
        if (matches[node] != -1) {
            continue;
        }
        for (size_t i = 0; i < graph.getNeighbors(node).size(); ++i) {
            int neighbor = graph.getNeighbors(node).at(i);
            if (matches[neighbor] == -1) {
                matches[node] = neighbor;
                matches[neighbor] = node;
                break;
            }
        }
    }

    // Kuhn algorithm
    for (size_t node = 0; node < graph.size(); ++node) {
        if (matches[node] == -1) {
            std::vector<bool> visited(graph.size(), false);
            augmentingPath(graph, matches, visited, node);
        }
    }

    return nodeMatchesToMatching(matches);
}


