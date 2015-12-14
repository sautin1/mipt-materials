#pragma once
#include <algorithm>
#include <cstddef>
#include <limits>
#include <stdexcept>
#include <vector>

struct Edge;
struct NeighborInfo;

using CostT = double;
using AdjacencyMatrix = std::vector<std::vector<CostT>>;
using AdjacencyList   = std::vector<std::vector<NeighborInfo>>;
using Matching        = std::vector<Edge>;

const CostT kDefaultEdgeCost = 1;
const CostT kInfinity = std::numeric_limits<CostT>::max();

struct NeighborInfo {
    int node;
    CostT cost;
    NeighborInfo() = default;
    NeighborInfo(int _node, CostT _cost) : node(_node), cost(_cost) {}
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

int nodeDegree(const Graph& graph, int node);
Graph minimalSpanningTree(const Graph& graph);
std::vector<int> dfsTimeIn(const Graph& graph);
int dfsTimeInNode(const Graph& graph,
                  int timer, int node,
                  std::vector<int>& time_in);
Graph createCycleOnSubgraph(const CompleteGraph& graph, const Graph& subgraph);
Graph induce(const Graph& graph, const std::vector<int>& induced_nodes);
Matching perfectMinWeightMatching(const Graph& graph);
Graph eulerianCycle(const Graph& graph);
