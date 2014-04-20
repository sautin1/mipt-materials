#include <fstream>
#include "undirected_graph.h"

const std::string INPUT_FILE  = "bridges.in";
const std::string OUTPUT_FILE = "bridges.out";

int main()
{
	std::ifstream fin;
	fin.open(INPUT_FILE.c_str(), std::ifstream::in);
	if (!fin.good()){
		throw std::runtime_error(std::string("Cannot open input file: ") + INPUT_FILE);
	}
	size_t node_quantity, edge_quantity;
	std::vector<edge_t> edges;
	fin >> node_quantity >> edge_quantity;
	for (size_t edge_index = 0; edge_index < edge_quantity; ++edge_index){
		size_t node1, node2;
		fin >> node1 >> node2;
		edges.push_back(edge_t(node1, node2, edge_index));
	}
	fin.close();
	undirected_graph graph(node_quantity, edge_quantity, edges);
	std::vector<size_t> bridges;
	std::vector<size_t> edge_component;
	graph.bridges(bridges, COMPONENT_MODE, edge_component);

	std::vector<size_t> cut_vertices;
	std::vector<size_t> node_component;
	graph.cut_vertices(cut_vertices, COMPONENT_MODE, node_component);

	std::ofstream fout;
	fout.open(OUTPUT_FILE.c_str(), std::ofstream::out);
	if (!fout.good()){
		throw std::runtime_error(std::string("Cannot create output file: ") + OUTPUT_FILE);
	}
	for (size_t bridge_index = 0; bridge_index < bridges.size(); ++bridge_index){
		fout << bridges[bridge_index] << ": " << edges[bridges[bridge_index]].first << "-" << edges[bridges[bridge_index]].second << "\n";
	}
	for (node_t node = 0; node < node_quantity; ++node){
		fout << edge_component[node] << " ";
	}
	fout << "\n\n";

	for (size_t cut_vertex_index = 0; cut_vertex_index < cut_vertices.size(); ++cut_vertex_index){
		fout << cut_vertices[cut_vertex_index] << "\n";
	}
	for (size_t edge_index = 0; edge_index < edge_quantity; ++edge_index){
		fout << node_component[edge_index] << " ";
	}
	fout.close();
	return 0;
}
