#include <iostream>
#include <vector>
#include "testing.h"

int main() {
//  testAll();
    std::vector<Edge> edges;
    edges.reserve(15);
    edges.emplace_back(1, 2, 1);
    edges.emplace_back(2, 3, 1);
    edges.emplace_back(3, 4, 1);
    edges.emplace_back(4, 5, 1);
    edges.emplace_back(3, 5, 1);
    edges.emplace_back(1, 3, 1);
    edges.emplace_back(3, 7, 1);
    edges.emplace_back(3, 6, 1);
    edges.emplace_back(7, 1, 1);
    edges.emplace_back(1, 6, 1);
    edges.emplace_back(7, 6, 1);
    edges.emplace_back(8, 6, 1);
    edges.emplace_back(7, 0, 1);
    edges.emplace_back(8, 0, 1);

    Graph graph(9, edges);
//  edges.reserve(6);
//  edges.emplace_back(0, 1, 1);
//  edges.emplace_back(0, 2, 1);
//  edges.emplace_back(2, 1, 1);
//  edges.emplace_back(2, 3, 1);
//  edges.emplace_back(2, 4, 1);
//  edges.emplace_back(3, 4, 1);
//  Graph graph(5, edges);
    std::vector<int> cycle = eulerianCycle(graph);
    for (int i = 0; i < cycle.size(); ++i) {
        std::cout << cycle[i] << ' ';
    }
    return 0;
}

