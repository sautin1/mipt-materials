// Copyright 2014, MIPT
// All rights reserved.
//
// Author: sautin1@yandex.ru (Andrew Sautin)
//
// The Suffix Tree C++ Library
//
// This header file declares the public API for searching substring occurrences in a string.

#ifndef SUBSTRINGS_H
#define SUBSTRINGS_H

#include <vector>

#include "suffix_tree.h"

typedef SuffixTree::LinkMapConstIterator LinkMapConstIterator;

// TraversalVisitor class for SuffixTree.DepthFirstSearchTraversal() method.
// Is designed for the needs of FindAllOccurences() function.
class FindOccurrencesTraversalVisitor {
public:
	FindOccurrencesTraversalVisitor(const SuffixTree& suffix_tree, const std::string& pattern);

	void InitVisitor();
	void DiscoverNode(const SuffixTree::Link& in_link);
	void ReturnToNode(const SuffixTree::Link& return_link, const SuffixTree::Link& in_link);
	void ExamineEdge(const SuffixTree::Link& link);
	LinkMapConstIterator ChooseNextNeighbour(int active_node, const LinkMapConstIterator& link_map_begin_it,
			const LinkMapConstIterator& link_map_next_letter_it, const LinkMapConstIterator& link_map_end_it);
	void FinishNode(const SuffixTree::Link& in_link);

	const std::vector<int>& occurrences() const;
private:
	const SuffixTree& suffix_tree_;
	std::string pattern_;
	std::vector<int> occurrences_;
	int pattern_index_;
	int pattern_end_node_;
	int suffix_length_;
};

// Returns vector of integers, that represent indices in suffix_tree sample,
// where search_string starts as a substring.
std::vector<int> FindAllOccurrences(const SuffixTree& suffix_tree, const std::string& search_string);

#endif // SUBSTRINGS_H
