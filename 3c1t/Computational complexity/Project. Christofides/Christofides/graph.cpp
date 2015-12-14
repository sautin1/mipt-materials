#include "graph.h"

bool Edge::operator< (const Edge other) const {
    return (cost < other.cost);
}

bool NeighborInfo::operator< (const NeighborInfo other) const {
    return (cost < other.cost);
}

Graph::Graph(int node_quantity, const std::vector<Edge>& edges)
    : nodes_(node_quantity, std::vector<NeighborInfo>()) {
    for (size_t i = 0; i < edges.size(); ++i) {
        nodes_[edges[i].from].push_back(NeighborInfo(edges[i].to  , edges[i].cost));
        nodes_[edges[i].to  ].push_back(NeighborInfo(edges[i].from, edges[i].cost));
    }
}

Graph::Graph(const AdjacencyMatrix& matrix)
    : nodes_(matrix.size(), std::vector<NeighborInfo>()) {
    for (size_t from = 0; from < matrix.size(); ++from) {
        for (size_t to = 0; to < matrix[from].size(); ++to) {
            if (matrix[from][to] < kInfinity && from != to) {
                nodes_[from].push_back(NeighborInfo(to, matrix[from][to]));
            }
        }
    }
}

size_t Graph::size() const {
    return nodes_.size();
}

const std::vector<NeighborInfo>& Graph::getNeighbors(int node) const {
    return nodes_.at(node);
}

const AdjacencyList& Graph::nodes() const {
    return nodes_;
}

std::vector<Edge> Graph::getEdges() const {
    std::vector<Edge> edges;
    for (int node = 0; node < (int)nodes_.size(); ++node) {
        for (size_t i = 0; i < nodes_[node].size(); ++i) {
            if (node < nodes_[node][i].node) {
                edges.push_back(Edge(node, nodes_[node][i].node, nodes_[node][i].cost));
            }
        }
    }
    return edges;
}

CostT CompleteGraph::getCost(int from, int to) const {
    if (from < to) {
        std::swap(from, to);
    }
    return nodes_[from][to].cost;
}

int nodeDegree(const Graph& graph, int node) {
    return graph.getNeighbors(node).size();
}

// Prim's algorithm without heap: O(v^2 + e)
Graph minimalSpanningTree(const Graph& graph) {
    std::vector<Edge> tree_edges;
    std::vector<int> node_queue;
    for (size_t i = 0; i < graph.size(); ++i) {
        node_queue.push_back(i);
    }
    std::vector<CostT> distances(graph.size(), kInfinity);
    distances[0] = 0;
    std::vector<NeighborInfo> parents(graph.size(), NeighborInfo(0, -1));

    while (!node_queue.empty()) {
        auto min_it = std::min_element(node_queue.begin(), node_queue.end(),
                                       [&](int left, int right) {
            return (distances[left] < distances[right]);
        });
        int min_node = *min_it;
        if (distances[min_node] == kInfinity) {
            throw std::logic_error("Graph is not connected");
        }
        if (parents[min_node].cost != -1) {
            tree_edges.push_back(Edge(min_node, parents[min_node]));
        }
        for (size_t j = 0; j < graph.getNeighbors(min_node).size(); ++j) {
            NeighborInfo neighbor = graph.nodes()[min_node][j];
            if (neighbor.cost < distances[neighbor.node]) {
                distances[neighbor.node] = neighbor.cost;
                parents  [neighbor.node] = NeighborInfo(min_node, neighbor.cost);
            }
        }
        *min_it = node_queue.back();
        node_queue.pop_back();
    }
    return Graph(graph.size(), tree_edges);
}

int dfsTimeInNode(const Graph& graph,
              int timer, int node,
              std::vector<int>& time_in) {
    time_in[node] = timer++;
    for (int i = 0; i < (int)graph.getNeighbors(node).size(); ++i) {
        int neighbor = graph.getNeighbors(node).at(i).node;
        if (time_in[neighbor] == -1) {
            timer = dfsTimeInNode(graph, timer, neighbor, time_in);
        }
    }
    return timer;
}

std::vector<int> dfsTimeIn(const Graph& graph) {
    std::vector<int> time_in(graph.size(), -1);
    int timer = 0;
    for (size_t node = 0; node < graph.size(); ++node) {
        if (time_in[node] == -1) {
            timer = dfsTimeInNode(graph, timer, node, time_in);
        }
    }
    return time_in;
}

Graph createCycleOnSubgraph(const CompleteGraph& graph, const Graph& subgraph) {
    std::vector<int> time_in = dfsTimeIn(subgraph);
    std::vector<int> node_sequence(subgraph.size(), -1);
    for (size_t node = 0; node < subgraph.size(); ++node) {
        node_sequence[time_in[node]] = node;
    }
    std::vector<Edge> edges;
    int from, to;
    for (size_t time = 0; time < node_sequence.size(); ++time) {
        from = node_sequence[time];
        to   = node_sequence[(time + 1) % node_sequence.size()];
        edges.push_back(Edge(from, to, graph.getCost(from, to)));
    }
    return Graph(subgraph.size(), edges);
}

Graph induce(const Graph& graph, const std::vector<int>& induced_nodes) {
    // todo
    return graph;
}

Matching perfectMinWeightMatching(const Graph& graph) {
    // todo
    return Matching();
}

Graph eulerianCycle(const Graph& graph) {
    // todo
    return graph;
}

