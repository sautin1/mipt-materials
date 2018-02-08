#include "test_system.h"

void tester()
{
	size_t node_quantity = (rand() % (MAX_CURRENCY_Q)) + 1;
	std::vector< std::vector<weight_t> > matrix(node_quantity, std::vector<weight_t>(node_quantity, INF));
	for (node_t node1 = 0; node1 < node_quantity; ++node1){
		for (node_t node2 = 0; node2 < node_quantity; ++node2){
			if (node1 == node2){
				matrix[node1][node2] = 1;
			} else if (node1 < node2){
				matrix[node1][node2] = (1.0*rand() / RAND_MAX) * MAX_CURRENCY_RATE;
			} else {
				double delta = 1.0 * (rand() % (MAX_CURRENCY_DELTA*2+1)) - MAX_CURRENCY_DELTA;
				matrix[node1][node2] = 1.0 / (matrix[node2][node1] + delta);
			}
		}
	}
	std::vector< std::vector<weight_t> > matrix_old(matrix);
	preprocess(node_quantity, matrix);

	weighted_graph graph(node_quantity, matrix);
	std::vector<node_t> cycle;
	bool is_cycle = graph.negative_cycle(cycle);
	double result = 1.0;
	if (is_cycle){
		for (size_t node_index = 1; node_index < cycle.size(); ++node_index){
			result *= matrix_old[cycle[node_index-1]][cycle[node_index]];
		}
		if (result < 1.0){
			throw std::logic_error(std::string("Found cycle doesn't provide any profit!"));
		}
	}
	std::cout << "Nodes: " << node_quantity << ". Cycle: " << is_cycle << ". Profit: " << result-1 << "\n";
}

void testArbitrage(size_t test_quantity)
{
	srand(time(NULL));
	for (size_t test_index = 0; test_index < test_quantity; ++test_index){
		tester();
		std::cout << "\t" << "Test #" << test_index << " passed!\n";
	}
	std::cout << "All tests passed!\n";
}

void testArbitrage()
{
	testArbitrage(TEST_Q);
}

