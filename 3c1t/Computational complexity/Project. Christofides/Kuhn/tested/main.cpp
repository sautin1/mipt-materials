#include <cstddef>
#include <iostream>
#include <vector>

struct Edge {
    int from;
    int to;
    Edge(int _from, int _to) : from(_from), to(_to) {}
};

class Graph {
public:
    Graph();
    Graph(int node_quantity, const std::vector<Edge>& edges);
    const std::vector<int>& getNeighbors(int node) const;
    size_t size() const;
private:
    std::vector<std::vector<int>> nodes_;
};

Graph::Graph() {}

Graph::Graph(int node_quantity, const std::vector<Edge>& edges)
    : nodes_(node_quantity, std::vector<int>()) {
    for (size_t i = 0; i < edges.size(); ++i) {
        nodes_[edges[i].from].push_back(edges[i].to);
        nodes_[edges[i].to].push_back(edges[i].from);
    }
}

const std::vector<int>& Graph::getNeighbors(int node) const {
    return nodes_[node];
}

size_t Graph::size() const {
    return nodes_.size();
}

using Matching = std::vector<Edge>;
using NodeMatches = std::vector<int>;

bool augmentingPath(const Graph& graph, NodeMatches& matching,
                    std::vector<bool>& visited, int node);
Matching nodeMatchesToMatching(const NodeMatches& matches);
Matching maximalMatchingBipartite(const Graph& graph);

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


int main() {
    int n, m;
    std::cin >> n >> m;
    std::vector<Edge> edges;
    for (int v1 = 0; v1 < n; ++v1) {
        int v2;
        std::cin >> v2;
        while (v2 > 0) {
            edges.push_back(Edge(v1, n + v2 - 1));
            std::cin >> v2;
        }
    }
    Graph graph(n + m, edges);
    Matching matching = maximalMatchingBipartite(graph);
    std::cout << matching.size() << std::endl;
    for (size_t i = 0; i < matching.size(); ++i) {
        std::cout << matching[i].from + 1 << ' ' << matching[i].to - n + 1 << std::endl;
    }
    return 0;
}

