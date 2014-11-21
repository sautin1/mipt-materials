#ifndef SUBSTRINGS_H
#define SUBSTRINGS_H

#include <vector>
#include "suffix_tree.h"

struct FindOccurrencesTraversalVisitor {
	SuffixTree suffix_tree;
	std::string search_string;
	std::vector<int> occurences;

	FindOccurrencesTraversalVisitor(const SuffixTree& _suffix_tree, const std::string& _search_string);
	FindOccurrencesTraversalVisitor(const FindOccurrencesTraversalVisitor& other_visitor);
	void InitVisitor();
	void DiscoverNode(int active_node);
	void ReturnToNode(int active_node);
	void ExamineEdge(int sample_start_index, int sample_end_index);
	int ChooseNextNeighbour(int active_node, char previous_neighbour_letter);
	void FinishNode(int active_node);
	void FinishEdge(int active_node);
	std::vector<int> GetOccurences() const;
};

std::vector<int> FindAllOccurrences(const SuffixTree& suffix_tree, const std::string& search_string);

#endif // SUBSTRINGS_H
