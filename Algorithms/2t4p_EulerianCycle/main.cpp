#include <fstream>
#include "directed_graph.h"
#include "eulerian_cycle_tester.h"

const std::string INPUT_FILE  = "eul_cycle.in";
const std::string OUTPUT_FILE = "eul_cycle.out";

int main()
{
	//Auto-test
	test_eulerian_cycle(TEST_QUANTITY);

	//User Test
	/*std::ifstream fin;
	fin.open(INPUT_FILE.c_str(), std::ifstream::in);
	if (!fin.good()){
		throw std::runtime_error(std::string("Cannot open input file: ") + INPUT_FILE);
	}
	size_t node_quantity, edge_quantity;
	node_t start_node;
	std::vector<edge_t> edges;
	fin >> node_quantity >> edge_quantity >> start_node;
	for (size_t edge_index = 0; edge_index < edge_quantity; ++edge_index){
		size_t node1, node2;
		fin >> node1 >> node2;
		edges.push_back(std::make_pair<node_t>(node1, node2));
	}
	fin.close();
	directed_graph graph(node_quantity, edges);
	std::vector<node_t> eulerian_cycle;
	graph.eulerian_cycle(eulerian_cycle, start_node);

	std::ofstream fout;
	fout.open(OUTPUT_FILE.c_str(), std::ofstream::out);
	for (std::vector<node_t>::iterator node_it = eulerian_cycle.begin(); node_it != eulerian_cycle.end(); ++node_it){
		fout << *node_it << " ";
	}
	fout.close();*/

	return 0;
}
