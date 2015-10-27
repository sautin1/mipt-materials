#include <iostream>
#include <vector>
#include "graph.h"
#include "matching.h"

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

