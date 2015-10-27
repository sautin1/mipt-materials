#include "graph.h"

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
