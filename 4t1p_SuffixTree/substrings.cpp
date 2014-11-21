#include "substrings.h"


FindOccurrencesTraversalVisitor::FindOccurrencesTraversalVisitor(const SuffixTree& _suffix_tree, const std::string& _search_string)
	: suffix_tree(_suffix_tree), search_string(_search_string) {}

FindOccurrencesTraversalVisitor::FindOccurrencesTraversalVisitor(const FindOccurrencesTraversalVisitor& other_visitor)
	: suffix_tree(other_visitor.suffix_tree), search_string(other_visitor.search_string) {}

void FindOccurrencesTraversalVisitor::InitVisitor() {
	occurences.clear();
}

void FindOccurrencesTraversalVisitor::DiscoverNode(int active_node) {

}

void FindOccurrencesTraversalVisitor::ReturnToNode(int active_node) {

}

void FindOccurrencesTraversalVisitor::ExamineEdge(int sample_start_index, int sample_end_index) {

}

int FindOccurrencesTraversalVisitor::ChooseNextNeighbour(int active_node, char previous_neighbour_letter) {

}

void FindOccurrencesTraversalVisitor::FinishNode(int active_node) {

}

void FindOccurrencesTraversalVisitor::FinishEdge(int active_node) {

}

std::vector<int> FindOccurrencesTraversalVisitor::GetOccurences() const {
	return occurences;
}


std::vector<int> FindAllOccurrences(const SuffixTree& suffix_tree, const std::string& search_string) {
	FindOccurrencesTraversalVisitor visitor(suffix_tree, search_string);
	suffix_tree.DepthFirstSearchTraversal<FindOccurrencesTraversalVisitor>(visitor);
	return visitor.GetOccurences();
}
