#ifndef SUBSTRINGS_H
#define SUBSTRINGS_H

#include <vector>
#include "suffix_tree.h"

typedef SuffixTree::LinkMapConstIterator LinkMapConstIterator;

class FindOccurrencesTraversalVisitor {
private:
	const SuffixTree& suffix_tree;
	std::string pattern;
	std::vector<int> occurrences;
	int pattern_index;
	int pattern_end_node;
	int suffix_length;
public:
	FindOccurrencesTraversalVisitor(const SuffixTree& _suffix_tree, const std::string& _pattern);

	void InitVisitor();
	void DiscoverNode(const SuffixTree::Link& in_link);
	void ReturnToNode(const SuffixTree::Link& return_link, const SuffixTree::Link& in_link);
	void ExamineEdge(const SuffixTree::Link& link);
	LinkMapConstIterator ChooseNextNeighbour(int active_node, const LinkMapConstIterator& link_map_begin_it,
			const LinkMapConstIterator& link_map_next_letter_it, const LinkMapConstIterator& link_map_end_it);
	void FinishNode(const SuffixTree::Link& in_link);

	std::vector<int> GetOccurrences() const;
};

std::vector<int> FindAllOccurrences(const SuffixTree& suffix_tree, const std::string& search_string);

#endif // SUBSTRINGS_H
