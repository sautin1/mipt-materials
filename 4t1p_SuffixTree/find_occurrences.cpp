#include "find_occurrences.h"

FindOccurrencesTraversalVisitor::FindOccurrencesTraversalVisitor(const SuffixTree& _suffix_tree, const std::string& _pattern)
	: suffix_tree(_suffix_tree), pattern(_pattern) {}

void FindOccurrencesTraversalVisitor::InitVisitor() {
	occurrences.clear();
	pattern_index = 0;
	suffix_length = 0;
	pattern_end_node = 0;
}

void FindOccurrencesTraversalVisitor::DiscoverNode(const SuffixTree::Link& in_link) {
	int active_node = in_link.target_node_index;
	if (pattern_end_node == 0 && pattern_index == (int)pattern.size()) {
		pattern_end_node = active_node;
	}
	if (pattern_end_node > 0 && suffix_tree.IsLeaf(active_node)) {
		occurrences.push_back(suffix_tree.sample().size() - suffix_length - pattern.size());
	}
}

void FindOccurrencesTraversalVisitor::ReturnToNode(const SuffixTree::Link& return_link, const SuffixTree::Link& in_link) {
	if (pattern_end_node > 0) {
		if (suffix_tree.IsLeaf(return_link.target_node_index)) {
			suffix_length -= suffix_tree.sample().size() - return_link.sample_start_index;
		} else {
			suffix_length -= return_link.sample_end_index - return_link.sample_start_index;
		}
	}
}

void FindOccurrencesTraversalVisitor::ExamineEdge(const SuffixTree::Link& link) {
	if (pattern_end_node > 0) {
		if (suffix_tree.IsLeaf(link.target_node_index)) {
			suffix_length += suffix_tree.sample().size() - link.sample_start_index;
		} else {
			suffix_length += link.sample_end_index - link.sample_start_index;
		}
	} else if (pattern_end_node == 0) {
		std::string sample = suffix_tree.sample();
		int sample_start_index = link.sample_start_index;
		int sample_end_index = std::min(link.sample_end_index, (int)sample.size());
		int sample_letter_index;
		for (sample_letter_index = sample_start_index + 1; sample_letter_index < sample_end_index; ++sample_letter_index) {
			if (pattern_index == (int)pattern.size()) {
				suffix_length += sample_end_index - sample_letter_index;
				break;
			}
			if (sample[sample_letter_index] != pattern[pattern_index]) {
				pattern_end_node = -1;
				break;
			}
			++pattern_index;
		}
	}
}

LinkMapConstIterator FindOccurrencesTraversalVisitor::ChooseNextNeighbour(int active_node, const LinkMapConstIterator& link_map_begin_it,
		const LinkMapConstIterator& link_map_next_letter_it, const LinkMapConstIterator& link_map_end_it) {
	if (pattern_end_node == -1) {
		return link_map_end_it;
	} else if (pattern_end_node == 0) {
		LinkMapConstIterator link_map_letter_it = suffix_tree.GetLinkIterator(active_node, pattern[pattern_index++]);
		if (link_map_letter_it == link_map_end_it) {
			pattern_end_node = -1;
		}
		return link_map_letter_it;
	} else {
		return link_map_next_letter_it;
	}
}

void FindOccurrencesTraversalVisitor::FinishNode(const SuffixTree::Link& in_link) {
	if (in_link.target_node_index == pattern_end_node) {
		pattern_end_node = -1;
	}
}

std::vector<int> FindOccurrencesTraversalVisitor::GetOccurrences() const {
	return occurrences;
}

std::vector<int> FindAllOccurrences(const SuffixTree& suffix_tree, const std::string& search_string) {
	FindOccurrencesTraversalVisitor visitor(suffix_tree, search_string);
	suffix_tree.DepthFirstSearchTraversal<FindOccurrencesTraversalVisitor>(visitor);
	return visitor.GetOccurrences();
}
