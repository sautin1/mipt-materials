#include "tsp.h"

Tour tspApproximationMST(const CompleteGraph& graph) {
    Graph mst = minimalSpanningTree(graph);
    Tour cycle = createHamiltonianCycle(mst);
    return cycle;
}

Tour tspApproximationChristofides(const CompleteGraph& graph) {
    Graph mst = minimalSpanningTree(graph);
    std::vector<int> odd_degree_nodes;
    for (size_t node = 0; node < mst.size(); ++node) {
        if (nodeDegree(mst, node) % 2 == 1) {
            odd_degree_nodes.push_back(node);
        }
    }

    InduceResult induce_result = induce(graph, odd_degree_nodes);
    Matching matching = perfectMinWeightMatching(induce_result.graph);
    for (Edge& edge : matching) {
        edge.from = induce_result.ind_to_init[edge.from];
        edge.to   = induce_result.ind_to_init[edge.to  ];
    }
    std::vector<Edge> mst_edges = mst.getEdges();
    std::vector<Edge> edges(matching.size() + mst_edges.size());
    auto it = std::copy(mst_edges.begin(), mst_edges.end(), edges.begin());
    std::copy(matching.begin(), matching.end(), it);

    std::vector<int> eulerian_cycle = eulerianCycle(Graph(graph.size(), edges));
    Tour cycle = createCycleOnTour(graph.size(), eulerian_cycle);
    return cycle;
}
