#include "tsp.h"

Graph tspApproximationMST(const CompleteGraph& graph) {
    Graph mst = minimalSpanningTree(graph);
    Graph cycle = createCycleOnSubgraph(graph, mst);
    return cycle;
}

Graph tspApproximationChristofides(const CompleteGraph& graph) {
    Graph mst = minimalSpanningTree(graph);
    std::vector<int> odd_degree_nodes;
    for (size_t node = 0; node < graph.size(); ++node) {
        if (nodeDegree(graph, node) % 2 == 1) {
            odd_degree_nodes.push_back(node);
        }
    }
    Graph induced = induce(graph, odd_degree_nodes);
    Matching match = perfectMinWeightMatching(graph);
    std::vector<Edge> edges(match.size() + mst.size());
    std::vector<Edge> mst_edges = mst.getEdges();
    auto it = std::copy(mst_edges.begin(), mst_edges.end(), edges.begin());
    std::copy(match.begin(), match.end(), it);

    std::vector<int> eulerian_cycle = eulerianCycle(graph);
//  Graph cycle = createCycleOnSubgraph(graph, eulerian_cycle);
//  return cycle;
    return Graph();
}
