#pragma once
#include <cstddef>
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
