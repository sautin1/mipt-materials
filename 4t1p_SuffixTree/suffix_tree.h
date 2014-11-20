#ifndef SUFFIX_TREE_H
#define SUFFIX_TREE_H

#include <string>
#include <vector>
#include <unordered_map>
#include <set>
#include <limits>

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
		std::vector<Link> links;
		int suffix_link_node_index;
		Node(int alphabet_size);
	};

	struct NodeReference {
		int closest_ancestor;
		int sample_start_index;
		int sample_end_index;
		NodeReference(int _ancestor_node, int _sample_start_index, int _sample_end_index);
	};

	struct TestAndSplitResult {
		bool reached_endpoint;
		int node_index; // if reached_endpoint is true, then node_index is end_point
		TestAndSplitResult(bool _is_split, int _node_index);
	};

	std::vector<Node> nodes_;
	int dummy_, root_;
	std::string sample_string_;
	std::unordered_map<char, int> alphabet_;

	int CreateNode();
	int GetLetterCode(int letter_index) const;
	void LinkNodes(int source_node_index, int target_node_index, int sample_start_index, int sample_end_index);
	bool HasLink(int node_index, int letter_index) const;
	Link GetLink(int node_index, int letter_index) const;

	void InitDummy();
	void InitAlphabet();
	void InitTree();

	void CanonicalizeNodeReference(NodeReference* node_reference) const;
	TestAndSplitResult TestAndSplit(const NodeReference& node_reference);

	NodeReference AddNewLetter(NodeReference active_point);

	void BuildTree();
public:
	SuffixTree(const std::string& sample);
	//DepthFirstSearch();
};

//std::vector<int> FindAllOccurrences(const SuffixTree& suffix_tree, const std::string& search_string);

#endif // SUFFIX_TREE_H
