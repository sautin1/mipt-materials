#ifndef SUFFIX_TREE_H
#define SUFFIX_TREE_H

#include <string>
#include <vector>
#include <map>
#include <set>
#include <limits>
#include <stdexcept>
#include <stack>
#include <gtest/gtest.h>

class SuffixTree
{
public:
	friend class TestSuffixTree;
	struct Link {
		int target_node_index;
		int sample_start_index, sample_end_index;
		Link();
		Link(int _target_node_index, int _sample_start_index, int _sample_end_index);
		bool operator == (const Link& link) const;
	};
	typedef std::map<char, Link>::const_iterator LinkMapConstIterator;
private:
	const int INF = std::numeric_limits<int>::max();
	struct Node {
		std::map<char, Link> links;
		int suffix_link_node_index;
		Node();
	};

	struct NodeReference {
		int closest_ancestor;
		int sample_start_index;
		int sample_end_index;
		NodeReference();
		NodeReference(int _ancestor_node, int _sample_start_index, int _sample_end_index);
		bool operator == (const NodeReference& node_reference) const;
	};

	struct TestAndSplitResult {
		bool reached_endpoint;
		int node_index; // if reached_endpoint is true, then node_index is end_point
		TestAndSplitResult();
		TestAndSplitResult(bool _reached_endpoint, int _node_index);
		bool operator == (const TestAndSplitResult& test_split_result) const;
	};

	struct DepthFirstSearchStackItem {
		Link enter_link;
		LinkMapConstIterator next_link_letter_it;
		DepthFirstSearchStackItem(const Link& _enter_link, LinkMapConstIterator _next_link_letter_it);
	};

	int CreateNode();
	void InitDummy(int sample_start_index, int sample_end_index);
	void InitLetterSet(int start_index, int sample_end_index);
	void InitTree();

	void LinkNodes(int source_node_index, int target_node_index, int sample_start_index, int sample_end_index);
	bool HasLink(int node_index, char letter) const;
	Link GetLink(int node_index, char letter) const;

	void CanonicalizeNodeReference(NodeReference* node_reference) const;
	TestAndSplitResult TestAndSplit(const NodeReference& node_reference);

	NodeReference AddNextLetter(NodeReference active_point);

	std::vector<Node> nodes_;
	std::set<char> letter_set_;
	int dummy_, root_;
	std::string sample_;
	NodeReference active_point_;
	char non_existing_char_;
public:
	SuffixTree();
	SuffixTree(const std::string& sample);
	SuffixTree(const SuffixTree& copy_tree);
	void UpdateNonExistingChar();

	void AppendSample(const std::string& append_sample);

	// API for visitor
	LinkMapConstIterator GetLinkIterator(int node_index, char letter) const;
	bool IsLeaf(int node_index) const;
	const std::string& sample() const;
	char non_existing_char() const;
	size_t Size() const;

	template <typename TraversalVisitor>
	void DepthFirstSearchTraversal(TraversalVisitor& visitor) const;

private:
	// testers for private methods
	friend class SuffixTreeTest;
	FRIEND_TEST(SuffixTreeTest, CreateNodeTest);
	FRIEND_TEST(SuffixTreeTest, InitDummyTest);
	FRIEND_TEST(SuffixTreeTest, InitLetterSetTest);
	FRIEND_TEST(SuffixTreeTest, InitTreeTest);
	FRIEND_TEST(SuffixTreeTest, LinkNodesTest);
	FRIEND_TEST(SuffixTreeTest, HasLinkTest);
	FRIEND_TEST(SuffixTreeTest, GetLinkTest);
	FRIEND_TEST(SuffixTreeTest, CanonicalizeNodeReferenceSimpleTest);
	FRIEND_TEST(SuffixTreeTest, CanonicalizeNodeReferenceNormalTest);
	FRIEND_TEST(SuffixTreeTest, TestAndSplitTest);
	FRIEND_TEST(SuffixTreeTest, AddNextLetterTestZero);
	FRIEND_TEST(SuffixTreeTest, AddNextLetterTestOne);
	FRIEND_TEST(SuffixTreeTest, AddNextLetterTestTwo);
	// testers for public methods
	FRIEND_TEST(SuffixTreeTest, UpdateNonExistingCharTest);
	FRIEND_TEST(SuffixTreeTest, AppendSampleTest);
	FRIEND_TEST(SuffixTreeTest, GetLinkIteratorTest);
	FRIEND_TEST(SuffixTreeTest, IsLeafTest);
	FRIEND_TEST(SuffixTreeTest, sampleTest);
	FRIEND_TEST(SuffixTreeTest, SizeTest);
	FRIEND_TEST(SuffixTreeTest, DepthFirstSearchTraversalTest);
	FRIEND_TEST(FindAllOccurrencesTest, StressTestCyclicString);
	FRIEND_TEST(FindAllOccurrencesTest, StressTestShuffledString);
};

template <typename TraversalVisitor>
void SuffixTree::DepthFirstSearchTraversal(TraversalVisitor& visitor) const {
	std::stack<DepthFirstSearchStackItem> dfs_stack;
	std::vector<bool> is_visited(nodes_.size(), false);
	Link last_used_link;
	dfs_stack.push(DepthFirstSearchStackItem(Link(root_, 0, 0), nodes_[root_].links.begin()));
	visitor.InitVisitor();
	while (!dfs_stack.empty()) {
		Link enter_link = dfs_stack.top().enter_link;
		int active_node = enter_link.target_node_index;
		LinkMapConstIterator next_link_letter_it = dfs_stack.top().next_link_letter_it;
		if (!is_visited[active_node]) {
			visitor.DiscoverNode(enter_link);
			is_visited[active_node] = true;
		} else {
			visitor.ReturnToNode(last_used_link, enter_link);
		}

		LinkMapConstIterator link_map_begin = nodes_[active_node].links.begin();
		LinkMapConstIterator link_map_end = nodes_[active_node].links.end();
		LinkMapConstIterator neighbour_letter_it = visitor.ChooseNextNeighbour(active_node, link_map_begin, next_link_letter_it, nodes_[active_node].links.end());
		if (neighbour_letter_it != link_map_end) {
			Link neighbour_link = neighbour_letter_it->second;
			visitor.ExamineEdge(neighbour_link);
			dfs_stack.top().next_link_letter_it = ++neighbour_letter_it;
			int target_node = neighbour_link.target_node_index;
			dfs_stack.push(DepthFirstSearchStackItem(neighbour_link, nodes_[target_node].links.begin()));
		} else {
			visitor.FinishNode(enter_link);
			last_used_link = dfs_stack.top().enter_link;
			dfs_stack.pop();
		}
	}
}

#endif // SUFFIX_TREE_H
