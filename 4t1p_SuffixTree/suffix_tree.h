#ifndef SUFFIX_TREE_H
#define SUFFIX_TREE_H

#include <string>
#include <vector>
#include <unordered_map>
#include <set>
#include <limits>
#include <stdexcept>
#include <stack> // for DFS

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

	typedef std::set<char>::iterator SetIterator;
	struct DepthFirstSearchStackItem {
		int node_index;
		char neighbour_letter;
		DepthFirstSearchStackItem(int _node_index, char _neighbour_letter);
	};

	std::vector<Node> nodes_;
	std::set<char> letter_set_;
	int dummy_, root_;
	std::string sample_;
	NodeReference active_point_;

	int CreateNode();
	void InitDummy(int sample_start_index, int sample_end_index);
	void InitLetterSet(int start_index, int sample_end_index);
	void InitTree();

	std::string sample() const;

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

	template <typename TraversalVisitor>
	void DepthFirstSearchTraversal(TraversalVisitor& visitor) const {
		std::stack<DepthFirstSearchStackItem> dfs_stack;
		std::vector<bool> is_visited(nodes_.size(), false);
		dfs_stack.push(DepthFirstSearchStackItem(root_, -1));
		visitor.InitVisitor();
		while (!dfs_stack.empty()) {
			int active_node = dfs_stack.top().node_index;
			char previous_neighbour_letter = dfs_stack.top().neighbour_letter;
			if (!is_visited[active_node]) {
				visitor.DiscoverNode(active_node);
				is_visited[active_node] = true;
			} else {
				visitor.ReturnToNode(active_node);
			}

			int neighbour_letter = visitor.ChooseNextNeighbour(active_node, previous_neighbour_letter);
			if (neighbour_letter != -1 && HasLink(active_node, neighbour_letter)) {
				Link link = GetLink(active_node, neighbour_letter);
				visitor.ExamineEdge(link.sample_start_index, link.sample_end_index);
				dfs_stack.top().neighbour_letter = neighbour_letter;
				dfs_stack.push(DepthFirstSearchStackItem(link.target_node_index, -1));
			} else {
				visitor.FinishNode(active_node);
				visitor.FinishEdge(active_node);
				dfs_stack.pop();
			}
		}
	}
};

#endif // SUFFIX_TREE_H
