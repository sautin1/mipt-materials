#ifndef SUFFIX_TREE_H
#define SUFFIX_TREE_H

#include <string>
#include <vector>
#include <unordered_map>
#include <set>
#include <limits>
#include <stdexcept>

#include <iostream>

class SuffixTree
{
private:
	const int INFINITY = std::numeric_limits<int>::max();
	struct Link {
		int target_node_index;
		int sample_start_index, sample_end_index;
		Link();
		Link(int _target_node_index, int _sample_start_index, int _sample_end_index);
	};

	struct Node {
		std::unordered_map<char, Link> links;
		int suffix_link_node_index;
		Node();
	};

	struct NodeReference {
		int closest_ancestor;
		int sample_start_index;
		int sample_end_index;
		NodeReference();
		NodeReference(int _ancestor_node, int _sample_start_index, int _sample_end_index);
	};

	struct TestAndSplitResult {
		bool reached_endpoint;
		int node_index; // if reached_endpoint is true, then node_index is end_point
		TestAndSplitResult(bool _is_split, int _node_index);
	};

	std::vector<Node> nodes_;
	std::set<char> letter_set_;
	int dummy_, root_;
	std::string sample_string_;
	NodeReference active_point_;

	int CreateNode();
	void InitDummy(int sample_start_index, int sample_end_index);
	void InitLetterSet(int start_index, int sample_end_index);
	void InitTree();

	void LinkNodes(int source_node_index, int target_node_index, int sample_start_index, int sample_end_index);
	Link GetLink(int node_index, char letter) const;
	bool HasLink(int node_index, char letter) const;

	void CanonicalizeNodeReference(NodeReference* node_reference) const;
	TestAndSplitResult TestAndSplit(const NodeReference& node_reference);

	NodeReference AddNextLetter(NodeReference active_point);
	void BuildTree();
public:
	SuffixTree(const std::string& sample);
	void AppendSample(const std::string& append_sample);
	void PrintTree(std::ostream& fout) const;
	bool IsSubstring(const std::string& substring) const;
	//DepthFirstSearch() const;
};

//std::vector<int> FindAllOccurrences(const SuffixTree& suffix_tree, const std::string& search_string);

#endif // SUFFIX_TREE_H
