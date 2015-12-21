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
        nodes_[edges[i].from].emplace_back(edges[i].to  , edges[i].cost,
                                           nodes_[edges[i].to].size());
        nodes_[edges[i].to  ].emplace_back(edges[i].from, edges[i].cost,
                                           nodes_[edges[i].from].size() - 1);
    }
}

Graph::Graph(const AdjacencyMatrix& matrix)
    : nodes_(matrix.size(), std::vector<NeighborInfo>()) {
    for (size_t from = 0; from < matrix.size() - 1; ++from) {
        for (size_t to = from + 1; to < matrix.size(); ++to) {
            if (matrix[from][to] < kInfinity) {
                nodes_[from].emplace_back(to  , matrix[from][to],
                                          nodes_[to].size());
                nodes_[to  ].emplace_back(from, matrix[from][to],
                                          nodes_[from].size() - 1);
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
                edges.emplace_back(node, nodes_[node][i].node, nodes_[node][i].cost);
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

std::vector<Edge> tourEdges(const CompleteGraph& graph, const Tour& tour) {
    std::vector<Edge> edges;
    for (int i = 0; i < (int)tour.size() - 1; ++i) {
        edges.emplace_back(tour[i], tour[i+1], graph.getCost(tour[i], tour[i+1]));
    }
    return edges;
}

CostT tourCost(const CompleteGraph& graph, const Tour& tour) {
    std::vector<Edge> tour_edges = tourEdges(graph, tour);
    CostT result = 0;
    for (Edge edge : tour_edges) {
        result += edge.cost;
    }
    return result;
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
    std::vector<NeighborInfo> parents(graph.size(), NeighborInfo(0, -1, -1));

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
            tree_edges.emplace_back(min_node, parents[min_node]);
        }
        for (size_t j = 0; j < graph.getNeighbors(min_node).size(); ++j) {
            NeighborInfo neighbor = graph.nodes()[min_node][j];
            if (neighbor.cost < distances[neighbor.node]) {
                distances[neighbor.node] = neighbor.cost;
                parents  [neighbor.node] = NeighborInfo(min_node, neighbor.cost, -1);
            }
        }
        *min_it = node_queue.back();
        node_queue.pop_back();
    }
    return Graph(graph.size(), tree_edges);
}

Tour createCycleOnTour(int node_quantity, const Tour& node_sequence) {
    Tour cycle;
    cycle.reserve(node_sequence.size());
    std::vector<bool> is_used(node_quantity, false);
    for (size_t i = 0; i < node_sequence.size(); ++i) {
        if (is_used[node_sequence[i]]) {
            continue;
        }
        is_used[node_sequence[i]] = true;
        cycle.push_back(node_sequence[i]);
    }
    cycle.push_back(node_sequence.front());
    return cycle;
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

Tour createHamiltonianCycle(const Graph& graph) {
    std::vector<int> time_in = dfsTimeIn(graph);
    Tour node_sequence(graph.size(), -1);
    for (size_t node = 0; node < graph.size(); ++node) {
        node_sequence[time_in[node]] = node;
    }
    return createCycleOnTour(graph.size(), node_sequence);
}

InduceResult induce(const Graph& graph, const std::vector<int>& induced_nodes) {
    InduceResult result(graph, induced_nodes);
    for (size_t i = 0; i < induced_nodes.size(); ++i) {
        int node = induced_nodes[i];
        result.init_to_ind[node] = i;
        result.ind_to_init[i] = node;
    }
    std::vector<Edge> edges = graph.getEdges();
    std::vector<Edge> edges_induced;
    for (Edge edge : edges) {
        if (result.init_to_ind[edge.from] != -1 && result.init_to_ind[edge.to] != -1) {
            Edge new_edge(result.init_to_ind[edge.from], result.init_to_ind[edge.to], edge.cost);
            edges_induced.push_back(new_edge);
        }
    }
    result.graph = Graph(induced_nodes.size(), edges_induced);
    return result;
}

Matching perfectMinWeightMatching(const Graph &graph) {
    std::vector<Edge> edges = graph.getEdges();
    std::sort(edges.begin(), edges.end());
    std::vector<bool> is_matched(graph.size(), false);
    Matching matching;
    for (Edge edge : edges) {
        if (!is_matched[edge.from] && !is_matched[edge.to]) {
            matching.push_back(edge);
            is_matched[edge.from] = true;
            is_matched[edge.to  ] = true;
        }
    }
    return matching;
}

Tour eulerianCycle(const Graph& graph) {
    std::vector<std::vector<bool>> is_used_edge;
    is_used_edge.reserve(graph.nodes().size());
    std::vector<int> neighbor_indices(graph.size(), -1);
    for (size_t i = 0; i < graph.size(); ++i) {
        is_used_edge.emplace_back(graph.nodes()[i].size(), false);
        neighbor_indices[i] = graph.getNeighbors(i).size() - 1;
    }
    Tour cycle;
    cycle.reserve(graph.size());
    std::stack<int> dfs_stack;
    dfs_stack.push(0);
    while (!dfs_stack.empty()) {
        int node = dfs_stack.top();
        while (neighbor_indices[node] >= 0
               && is_used_edge[node][neighbor_indices[node]]) {
            --neighbor_indices[node];
        }
        if (neighbor_indices[node] < 0) {
            // no more edges going from this node
            cycle.push_back(node);
            dfs_stack.pop();
        } else {
            NeighborInfo info = graph.getNeighbors(node)[neighbor_indices[node]];
            int neighbor = info.node;
            is_used_edge[node][neighbor_indices[node]] = true;
            is_used_edge[neighbor][info.link] = true;
            --neighbor_indices[node];
            dfs_stack.push(neighbor);
        }
    }
    return cycle;
}

