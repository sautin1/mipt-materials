#include "testing.h"

CostT euclideanDistance(const Point& a, const Point& b) {
    return std::sqrt(std::pow(a.x - b.x, 2) + std::pow(a.y - b.y, 2));
}

CompleteGraph readNonPlanarCompleteGraph(const std::string& filename) {
    std::ifstream fin(filename);
    if (!fin) {
        throw std::runtime_error("File " + filename + " cannot be opened");
    }
    int node_quantity;
    fin >> node_quantity;
    AdjacencyMatrix matrix(node_quantity, std::vector<CostT>(node_quantity, 0));
    for (int i = 0; i < node_quantity; ++i) {
        for (int j = 0; j < node_quantity; ++j) {
            fin >> matrix[i][j];
        }
    }
    return CompleteGraph(matrix);
}

CompleteGraph readPlanarCompleteGraph(const std::string& filename) {
    std::vector<Point> points;
    std::ifstream fin(filename);
    if (!fin) {
        throw std::runtime_error("File " + filename + " cannot be opened");
    }
    int node_quantity;
    fin >> node_quantity;
    for (int i = 0; i < node_quantity; ++i) {
        CostT x, y;
        fin >> x >> y;
        points.emplace_back(x, y);
    }
    fin.close();
    AdjacencyMatrix matrix(node_quantity, std::vector<CostT>(node_quantity, 0));
    for (int i = 0; i < node_quantity; ++i) {
        for (int j = 0; j < i; ++j) {
            matrix[i][j] = euclideanDistance(points[i], points[j]);
            matrix[j][i] = matrix[i][j];
        }
    }
    return CompleteGraph(matrix);
}

CostT readResult(const std::string& filename) {
    std::ifstream fin(filename);
    if (!fin) {
        throw std::runtime_error("File " + filename + " cannot be opened");
    }
    CostT result;
    fin >> result;
    fin.close();
    return result;
}

std::string generateFilename(const std::string& testname, bool is_input) {
    return kFilePath + testname + (is_input? kFileInputSuffix : kFileOutputSuffix);
}

bool test(const Test& test_case) {
    std::string filename = generateFilename(test_case.name, true);
    CompleteGraph graph = test_case.is_planar ? readPlanarCompleteGraph(filename)
                                              : readNonPlanarCompleteGraph(filename);
    std::cout << std::setw(kColumnWidth) << std::left
              << "Node quantity: " << graph.size()  << std::endl;
    Graph cycle = tspApproximationMST(graph);
    CostT result_approx = 0;
    for (Edge edge : cycle.getEdges()) {
        result_approx += edge.cost;
    }

    CostT result_exact = readResult(generateFilename(test_case.name, false));
    std::cout << std::setw(kColumnWidth) << std::left
              << "Exact result: "       << result_exact  << std::endl;
    std::cout << std::setw(kColumnWidth) << std::left
              << "Approximate result: " << result_approx << std::endl;
    std::cout << std::setw(kColumnWidth) << std::left
              << "Approx. ratio: " << (1.0 * result_approx) / result_exact << std::endl;
    return (result_approx <= 2 * result_exact);
}

void testAll() {
    int tests_passed = 0;
    for (const Test& test_case : kTestSet) {
        std::cout << std::setw(kColumnWidth) << std::left
                  << "Test name: " << test_case.name << std::endl;
        std::cout << std::setw(kColumnWidth) << std::left
                  << "Planar: " << (test_case.is_planar ? "yes" : "no") << std::endl;
        bool result = false;
        try {
            result = test(test_case);
        } catch (std::runtime_error& err) {
            std::cout << err.what() << std::endl;
        }
        std::cout << std::setw(kColumnWidth) << std::left << "Feedback: ";
        if (result) {
            std::cout << kAnsiiGreenBold << "OK"   << kAnsiiDefault << std::endl;
            ++tests_passed;
        } else {
            std::cout << kAnsiiRedBold   << "FAIL" << kAnsiiDefault << std::endl;
        }
        std::cout << std::endl;
    }
    std::cout << "Tests passed: " << tests_passed << " / " << kTestSet.size() << std::endl;
}
