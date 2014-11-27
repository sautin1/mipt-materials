// Copyright 2014, MIPT
// All rights reserved.
//
// Author: sautin1@yandex.ru (Andrew Sautin)
//
// The Suffix Tree C++ Library
//
// This header file declares the public API for working with suffix tree.

#ifndef SUFFIX_TREE_H
#define SUFFIX_TREE_H

#include <limits>
#include <map>
#include <set>
#include <stack>
#include <stdexcept>
#include <string>
#include <vector>

#include <gtest/gtest.h>

// Class SuffixTree represents suffix tree,
// which is a compressed trie containing all the suffixes of the given sample.
class SuffixTree {
 public:
  // Class Link represents a simple directed link in the suffix tree.
  // It is a link, connecting two nodes of the suffix tree and
  // storing a label (substring of a sample) on it.
  struct Link {
    int target_node_index;
    int sample_start_index, sample_end_index;
    Link();
    Link(int _target_node_index, int _sample_start_index, int _sample_end_index);
    bool Equals(const Link& link) const;
  };
  typedef std::map<char, Link>::const_iterator LinkMapConstIterator;

  // Index which is beforehand greater than every index of sample.
  static const int kInfinity = std::numeric_limits<int>::max();

  // Constructs suffix tree based on empty sample. No additional symbols are added.
  // It is supposed to use AppendSample() method afterwards.
  SuffixTree();
  // Constructs suffix tree based on sample with non_existing_char_ at the end of a string.
  explicit SuffixTree(const std::string& sample);
  // Copy constructor.
  SuffixTree(const SuffixTree& suffix_tree) = default;
  // Assignment operator.
  SuffixTree& operator = (const SuffixTree& other) = default;

  // Updates non_existing_char_:
  // if non_existing_char_ exists in sample_, then the character with the lowest code
  // which doesn't exist in sample_ will be assigned to non_existing_char.
  void UpdateNonExistingChar();
  // Appends append_sample to already constructed tree.
  // It is supposed to be used after the call of default constructor.
  void AppendSample(const std::string& append_sample);

  // Returns suffix tree link, which starts in node with number node_index and
  // has label starting with letter.
  LinkMapConstIterator GetLinkIterator(int node_index, char letter) const;
  // Returns true, if node with number node_index is a leaf (has no outcoming links).
  bool IsLeaf(int node_index) const;
  // Returns const reference to sample_.
  const std::string& sample() const;
  // Returns non_existing_char_.
  char non_existing_char() const;
  // Returns the number of nodes in the suffix tree.
  size_t Size() const;

  // Traverses the suffix tree and calls methods of visitor on special events.
  // Can be widely used for solving problems on strings.
  // TraversalVisitor is a class, which must have such methods declared:
  // 1) void InitVisitor()
  //    is called before the start of the traversal.
  // 2) void DiscoverNode(const SuffixTree::Link& in_link)
  //    is called when a node is visited for the first time.
  // 3) void ReturnToNode(const SuffixTree::Link& return_link, const SuffixTree::Link& in_link)
  //    is called when a node is visited not for the first time.
  // 4) void ExamineEdge(const SuffixTree::Link& link)
  //    is called before going through a link.
  // 5) LinkMapConstIterator ChooseNextNeighbour(int active_node,
  //      const LinkMapConstIterator& link_map_begin_it,
  //      const LinkMapConstIterator& link_map_next_letter_it,
  //      const LinkMapConstIterator& link_map_end_it)
  //    is called when choosing the node to go next to.
  // 6) void FinishNode(const SuffixTree::Link& in_link)
  //    is called before leaving the node forever.
  // As a result, visitor can completely control the flow of the traversal.
  template <typename TraversalVisitor>
  void DepthFirstSearchTraversal(TraversalVisitor* visitor) const;

 private:
  // Struct Node represents a simple explicit node in the suffix tree.
  // A map of outgoing links and the target node number of the suffix link
  // are stored in Node.
  struct Node {
    std::map<char, Link> links;
    int suffix_link_node_index;
    Node();
  };

  // Struct NodeReference represents a place where node (either explicit or implicit)
  // can be met in the suffix tree.
  // This place is represented by:
  // 1) closest_ancestor - the number of the closest explicit node on the way
  // from root_ node to the place;
  // 2) two indices, representing the substring of the label on the link
  // from closest_ancestor to the place.
  struct NodeReference {
    int closest_ancestor;
    int sample_start_index;
    int sample_end_index;
    NodeReference();
    NodeReference(int _ancestor_node, int _sample_start_index, int _sample_end_index);
    bool Equals(const NodeReference& node_reference) const;
  };

  // Elements of type TestAndSplitResult are returned by TestAndSplit() method.
  struct TestAndSplitResult {
    bool reached_endpoint;
    int node_index;
    TestAndSplitResult();
    TestAndSplitResult(bool _reached_endpoint, int _node_index);
    bool Equals(const TestAndSplitResult& test_split_result) const;
  };

  // Elements of type DepthFirstSearchStackItem are stored in dfs_stack in
  // DepthFirstSearchTraversal() method.
  struct DepthFirstSearchStackItem {
    Link enter_link;
    LinkMapConstIterator next_link_letter_it;
    DepthFirstSearchStackItem(const Link& _enter_link,
                              const LinkMapConstIterator& _next_link_letter_it);
  };

  // Creates new node in the suffix tree and returns its number.
  int CreateNode();
  // Links dummy_ node with root_ node by every letter, existing in sample_, and non_existing_char_.
  void InitDummy(int sample_start_index, int sample_end_index);
  // Fills the set of only those letters that exist in sample_.
  void InitLetterSet(int start_index, int sample_end_index);
  // Initializes empty suffix tree.
  void InitTree();

  // Creates link from source_node_index node to target_node_index node
  // with label sample_[sample_start_index..sample_end_index].
  void LinkNodes(int source_node_index, int target_node_index,
                 int sample_start_index, int sample_end_index);
  // Returns true, if there is a link from node_index node with label, starting with letter.
  bool HasLink(int node_index, char letter) const;
  // Returns link from node_index node with label, starting with letter.
  Link GetLink(int node_index, char letter) const;

  // If node_reference represents an explicit node, then node_reference remains unchanged.
  // Otherwise, method returns the last explicit node on the part of the link from
  // node_reference.closest_ancestor by
  // label sample_[node_reference.sample_start_index...node_reference.sample_end_index).
  void CanonicalizeNodeReference(NodeReference* node_reference) const;
  // Checks if there is a link from node_reference.closest_ancestor
  // by label sample_[node_reference.sample_start_index..node_reference.sample_end_index]
  // and splits the link into two parts by creating a split-node if node_reference
  // represents an implicit node and there is no such link.
  TestAndSplitResult TestAndSplit(const NodeReference& node_reference);
  // Appends the next letter of sample_ to already constructed tree.
  // Runs through the nodes by suffix links between active_point till end_point,
  // adding new links and nodes.
  // Returns endpoint.
  // It is one step of creating a tree or appending sample.
  NodeReference AddNextLetter(NodeReference active_point);

  // Vector of all nodes of the suffix_tree.
  std::vector<Node> nodes_;
  // Set of all chars, existing in sample_.
  std::set<char> letter_set_;
  // Numbers of two main nodes of the suffix tree.
  int dummy_, root_;
  // The string our suffix tree is built on.
  std::string sample_;
  // The place of last endpoint in the construction of the tree
  // (see Ukkonen's algorithm for more details).
  NodeReference active_point_;
  // Character, which does not exist in the sample_.
  char non_existing_char_;

  friend class SuffixTreeTest;
  // testers for private methods
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
void SuffixTree::DepthFirstSearchTraversal(TraversalVisitor* visitor) const {
  std::stack<DepthFirstSearchStackItem> dfs_stack;
  std::vector<bool> is_visited(nodes_.size(), false);
  Link last_used_link;
  dfs_stack.push(DepthFirstSearchStackItem(Link(root_, 0, 0), nodes_[root_].links.begin()));
  visitor->InitVisitor();
  while (!dfs_stack.empty()) {
    Link enter_link = dfs_stack.top().enter_link;
    int active_node = enter_link.target_node_index;
    LinkMapConstIterator next_link_letter_it = dfs_stack.top().next_link_letter_it;
    if (!is_visited[active_node]) {
      visitor->DiscoverNode(enter_link);
      is_visited[active_node] = true;
    } else {
      visitor->ReturnToNode(last_used_link, enter_link);
    }

    LinkMapConstIterator link_map_begin = nodes_[active_node].links.begin();
    LinkMapConstIterator link_map_end = nodes_[active_node].links.end();
    LinkMapConstIterator neighbour_letter_it;
    neighbour_letter_it = visitor->ChooseNextNeighbour(active_node,
                                                      link_map_begin,
                                                      next_link_letter_it,
                                                      nodes_[active_node].links.end());
    if (neighbour_letter_it != link_map_end) {
      Link neighbour_link = neighbour_letter_it->second;
      visitor->ExamineEdge(neighbour_link);
      dfs_stack.top().next_link_letter_it = ++neighbour_letter_it;
      int target_node = neighbour_link.target_node_index;
      dfs_stack.push(DepthFirstSearchStackItem(neighbour_link, nodes_[target_node].links.begin()));
    } else {
      visitor->FinishNode(enter_link);
      last_used_link = dfs_stack.top().enter_link;
      dfs_stack.pop();
    }
  }
}

#endif  // SUFFIX_TREE_H
