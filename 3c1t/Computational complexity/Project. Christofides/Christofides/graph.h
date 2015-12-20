#pragma once
#include <algorithm>
#include <cstddef>
#include <limits>
#include <stack>
#include <stdexcept>
#include <vector>

struct Edge;
struct NeighborInfo;

using CostT = double;
using AdjacencyMatrix = std::vector<std::vector<CostT>>;
using AdjacencyList   = std::vector<std::vector<NeighborInfo>>;
using Matching        = std::vector<Edge>;
using Tour            = std::vector<int>;

const CostT kDefaultEdgeCost = 1;
const CostT kInfinity = std::numeric_limits<CostT>::max();

struct NeighborInfo {
    int node;
    CostT cost;
    int link; // reverse edge link
    NeighborInfo() = default;
    NeighborInfo(int _node, CostT _cost, int _link) : node(_node), cost(_cost), link(_link) {}
    bool operator < (const NeighborInfo other) const;
};

struct Edge {
    int from;
    int to;
    CostT cost;
    Edge() = default;
    Edge(int _from, int _to, CostT _cost = kDefaultEdgeCost) : from(_from), to(_to), cost(_cost) {}
    Edge(int node, NeighborInfo neighbor) : from(node), to(neighbor.node), cost(neighbor.cost) {}
    bool operator < (const Edge other) const;
};

class Graph {
public:
    Graph() = default;
    Graph(int node_quantity, const std::vector<Edge>& edges);
    explicit Graph(const AdjacencyMatrix& matrix);

    // small getter-like methods
    size_t size() const;
    const std::vector<NeighborInfo>& getNeighbors(int node) const;
    const AdjacencyList& nodes() const;
    std::vector<Edge> getEdges() const;
protected:
    AdjacencyList nodes_;
};

class CompleteGraph : public Graph {
public:
    explicit CompleteGraph(AdjacencyMatrix matrix) : Graph(matrix) {}
    CostT getCost(int from, int to) const;
};

struct InduceResult {
    Graph graph;                  // induced graph
    std::vector<int> init_to_ind; // maps nodes in initial graph to nodes in induced graph
    std::vector<int> ind_to_init; // maps nodes in induces graph to nodes in initial graph
    InduceResult(const Graph& graph, const std::vector<int>& induced_nodes)
        : init_to_ind(graph.size(), -1), ind_to_init(induced_nodes.size(), -1) {}
};

int nodeDegree(const Graph& graph, int node);
std::vector<Edge> tourEdges(const CompleteGraph& graph, const Tour& tour);
CostT tourCost(const CompleteGraph& graph, const Tour& tour);

Graph minimalSpanningTree(const Graph& graph);
std::vector<int> dfsTimeIn(const Graph& graph);
int dfsTimeInNode(const Graph& graph,
                  int timer, int node,
                  std::vector<int>& time_in);
Tour createCycleOnTour(int node_quantity, const Tour& node_sequence);
Tour createHamiltonianCycle(const Graph& graph);
InduceResult induce(const Graph& graph, const std::vector<int>& induced_nodes);
Matching perfectMinWeightMatching(const Graph& graph);

Tour eulerianCycle(const Graph& graph);
