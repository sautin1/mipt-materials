// Copyright 2014, MIPT
// All rights reserved.
//
// Author: sautin1@yandex.ru (Andrew Sautin)
//
// The Suffix Tree C++ Library Tester
//
// This header file declares the public API for testing API from "suffix_tree.h".

#ifndef TEST_suffix_tree_H
#define TEST_suffix_tree_H

#include "suffix_tree.h"

#include <algorithm>

#include <gtest/gtest.h>

class SuffixTreeTest : public ::testing::Test {
 protected:
  void CheckBananaTree() {
    ASSERT_EQ(suffix_tree_.nodes_.size(), 12);

    ASSERT_EQ(suffix_tree_.dummy_, 0);
    ASSERT_EQ(suffix_tree_.nodes_[0].links.size(), 4);
    ASSERT_EQ(suffix_tree_.nodes_[0].suffix_link_node_index, -1);
    for (size_t letter_index = 0; letter_index < suffix_tree_.sample_.size(); ++letter_index) {
      ASSERT_NO_THROW(suffix_tree_.nodes_[0].links.at(suffix_tree_.sample_[letter_index]));
    }

    ASSERT_EQ(suffix_tree_.root_, 1);
    ASSERT_EQ(suffix_tree_.nodes_[1].links.size(), 4);
    ASSERT_EQ(suffix_tree_.nodes_[1].suffix_link_node_index, 0);
    SuffixTree::Link link;
    ASSERT_NO_THROW(link = suffix_tree_.nodes_[1].links.at('b'));
    ASSERT_TRUE(link.Equals(SuffixTree::Link(2, 0, suffix_tree_.kInfinity)));
    ASSERT_NO_THROW(link = suffix_tree_.nodes_[1].links.at('a'));
    ASSERT_TRUE(link.Equals(SuffixTree::Link(9, 1, 2)));
    ASSERT_NO_THROW(link = suffix_tree_.nodes_[1].links.at('n'));
    ASSERT_TRUE(link.Equals(SuffixTree::Link(7, 2, 4)));
    ASSERT_NO_THROW(link = suffix_tree_.nodes_[1].links.at('$'));
    ASSERT_TRUE(link.Equals(SuffixTree::Link(11, 6, suffix_tree_.kInfinity)));

    ASSERT_EQ(suffix_tree_.nodes_[2].links.size(), 0);
    ASSERT_EQ(suffix_tree_.nodes_[2].suffix_link_node_index, -1);

    ASSERT_EQ(suffix_tree_.nodes_[3].links.size(), 0);
    ASSERT_EQ(suffix_tree_.nodes_[3].suffix_link_node_index, -1);

    ASSERT_EQ(suffix_tree_.nodes_[4].links.size(), 0);
    ASSERT_EQ(suffix_tree_.nodes_[4].suffix_link_node_index, -1);

    ASSERT_EQ(suffix_tree_.nodes_[5].links.size(), 2);
    ASSERT_EQ(suffix_tree_.nodes_[5].suffix_link_node_index, 7);
    ASSERT_NO_THROW(link = suffix_tree_.nodes_[5].links.at('$'));
    ASSERT_TRUE(link.Equals(SuffixTree::Link(6, 6, suffix_tree_.kInfinity)));
    ASSERT_NO_THROW(link = suffix_tree_.nodes_[5].links.at('n'));
    ASSERT_TRUE(link.Equals(SuffixTree::Link(3, 4, suffix_tree_.kInfinity)));

    ASSERT_EQ(suffix_tree_.nodes_[6].links.size(), 0);
    ASSERT_EQ(suffix_tree_.nodes_[6].suffix_link_node_index, -1);

    ASSERT_EQ(suffix_tree_.nodes_[7].links.size(), 2);
    ASSERT_EQ(suffix_tree_.nodes_[7].suffix_link_node_index, 9);
    ASSERT_NO_THROW(link = suffix_tree_.nodes_[7].links.at('$'));
    ASSERT_TRUE(link.Equals(SuffixTree::Link(8, 6, suffix_tree_.kInfinity)));
    ASSERT_NO_THROW(link = suffix_tree_.nodes_[7].links.at('n'));
    ASSERT_TRUE(link.Equals(SuffixTree::Link(4, 4, suffix_tree_.kInfinity)));

    ASSERT_EQ(suffix_tree_.nodes_[8].links.size(), 0);
    ASSERT_EQ(suffix_tree_.nodes_[8].suffix_link_node_index, -1);

    ASSERT_EQ(suffix_tree_.nodes_[9].links.size(), 2);
    ASSERT_EQ(suffix_tree_.nodes_[9].suffix_link_node_index, 1);
    ASSERT_NO_THROW(link = suffix_tree_.nodes_[9].links.at('$'));
    ASSERT_TRUE(link.Equals(SuffixTree::Link(10, 6, suffix_tree_.kInfinity)));
    ASSERT_NO_THROW(link = suffix_tree_.nodes_[9].links.at('n'));
    ASSERT_TRUE(link.Equals(SuffixTree::Link(5, 2, 4)));

    ASSERT_EQ(suffix_tree_.nodes_[10].links.size(), 0);
    ASSERT_EQ(suffix_tree_.nodes_[10].suffix_link_node_index, -1);

    ASSERT_EQ(suffix_tree_.nodes_[11].links.size(), 0);
    ASSERT_EQ(suffix_tree_.nodes_[11].suffix_link_node_index, -1);
  }

  void CheckCorrectDummy() {
    ASSERT_LT(suffix_tree_.dummy_, suffix_tree_.nodes_.size());
    ASSERT_EQ(suffix_tree_.dummy_, 0);
    for (size_t letter_index = 0; letter_index < suffix_tree_.sample_.size(); ++letter_index) {
      char letter = suffix_tree_.sample_[letter_index];
      ASSERT_NO_THROW(suffix_tree_.nodes_[suffix_tree_.dummy_].links.at(letter));
    }
    ASSERT_EQ(suffix_tree_.nodes_[0].suffix_link_node_index, -1);
  }

  void CheckCorrectRoot() {
    ASSERT_LT(suffix_tree_.root_, suffix_tree_.nodes_.size());
    ASSERT_EQ(suffix_tree_.root_, 1);
    for (size_t letter_index = 0; letter_index < suffix_tree_.sample_.size(); ++letter_index) {
      char letter = suffix_tree_.sample_[letter_index];
      ASSERT_NO_THROW(suffix_tree_.nodes_[suffix_tree_.root_].links.at(letter));
    }
    ASSERT_EQ(suffix_tree_.nodes_[1].suffix_link_node_index, 0);
  }

  void CheckLetterSet() {
    for (size_t letter_index = 0; letter_index < suffix_tree_.sample_.size(); ++letter_index) {
      std::set<char>::const_iterator letter_set_char_it;
      letter_set_char_it = suffix_tree_.letter_set_.find(suffix_tree_.sample_[letter_index]);
      ASSERT_NE(letter_set_char_it, suffix_tree_.letter_set_.end());
    }
  }

  SuffixTree suffix_tree_;
};

TEST_F(SuffixTreeTest, CreateNodeTest) {
  int old_suffix_tree_size = suffix_tree_.nodes_.size();
  for (int insert_index = 0; insert_index < 100; ++insert_index) {
    int created_node_index = suffix_tree_.CreateNode();
    ASSERT_EQ(created_node_index, old_suffix_tree_size + insert_index);
    ASSERT_EQ(suffix_tree_.nodes_.size(), old_suffix_tree_size + insert_index + 1);
    ASSERT_EQ(suffix_tree_.nodes_[created_node_index].suffix_link_node_index, -1);
  }
}

TEST_F(SuffixTreeTest, InitDummyTest) {
  int dummy = suffix_tree_.dummy_;
  suffix_tree_.nodes_[dummy].links.clear();
  suffix_tree_.sample_ = "abcdefghijklmnopqrstuvwxyz";
  // expected to add links
  suffix_tree_.InitDummy(0, suffix_tree_.sample_.size());
  CheckCorrectDummy();

  // expected not to overwrite links
  size_t old_link_quantity = suffix_tree_.nodes_[dummy].links.size();
  suffix_tree_.sample_ += suffix_tree_.sample_;
  size_t sample_start_index = old_link_quantity;
  for (; sample_start_index < suffix_tree_.sample_.size(); ++sample_start_index) {
    char old_link_letter = suffix_tree_.sample_[sample_start_index];
    SuffixTree::Link old_link = suffix_tree_.nodes_[dummy].links[old_link_letter];
    suffix_tree_.InitDummy(sample_start_index, sample_start_index + 1);
    char sample_start_letter = suffix_tree_.sample_[sample_start_index];
    SuffixTree::Link new_link;
    ASSERT_NO_THROW(new_link = suffix_tree_.nodes_[dummy].links.at(sample_start_letter));
    ASSERT_TRUE(old_link.Equals(new_link));
    ASSERT_EQ(suffix_tree_.nodes_[dummy].links.size(), old_link_quantity);
  }
}

TEST_F(SuffixTreeTest, InitLetterSetTest) {
  suffix_tree_.letter_set_.clear();
  suffix_tree_.sample_ = "abcdefghijklmnopqrstuvwxyz";
  suffix_tree_.InitLetterSet(0, suffix_tree_.sample_.size());
  CheckLetterSet();
}

TEST_F(SuffixTreeTest, InitTreeTest) {
  suffix_tree_.nodes_.clear();
  suffix_tree_.InitTree();
  CheckCorrectDummy();
  CheckCorrectRoot();
  SuffixTree::NodeReference correct_node_reference(suffix_tree_.root_, 0, 0);
  ASSERT_TRUE(suffix_tree_.active_point_.Equals(correct_node_reference));
}

TEST_F(SuffixTreeTest, LinkNodesTest) {
  suffix_tree_.nodes_[suffix_tree_.root_].links.clear();
  suffix_tree_.LinkNodes(suffix_tree_.root_, suffix_tree_.dummy_, 0, 1);
  ASSERT_EQ(suffix_tree_.nodes_[suffix_tree_.root_].links.size(), 1);
  SuffixTree::Link link;
  ASSERT_NO_THROW(link = suffix_tree_.nodes_[suffix_tree_.root_].links.at(suffix_tree_.sample_[0]));
  ASSERT_TRUE(link.Equals(SuffixTree::Link(suffix_tree_.dummy_, 0, 1)));
}

TEST_F(SuffixTreeTest, HasLinkTest) {
  suffix_tree_.nodes_[suffix_tree_.dummy_].links.clear();
  suffix_tree_.nodes_[suffix_tree_.root_].links.clear();
  suffix_tree_.LinkNodes(suffix_tree_.root_, suffix_tree_.dummy_, 0, 1);
  ASSERT_TRUE(suffix_tree_.HasLink(suffix_tree_.root_, suffix_tree_.sample_[0]));
  ASSERT_FALSE(suffix_tree_.HasLink(suffix_tree_.dummy_, suffix_tree_.sample_[0]));
  suffix_tree_.nodes_[suffix_tree_.root_].links.erase(suffix_tree_.sample_[0]);
  ASSERT_FALSE(suffix_tree_.HasLink(suffix_tree_.root_, suffix_tree_.sample_[0]));
}

TEST_F(SuffixTreeTest, GetLinkTest) {
  suffix_tree_.nodes_[suffix_tree_.root_].links.clear();
  suffix_tree_.nodes_[suffix_tree_.dummy_].links.clear();
  ASSERT_THROW(suffix_tree_.GetLink(suffix_tree_.root_, suffix_tree_.sample_[0]),
               std::out_of_range);
  suffix_tree_.LinkNodes(suffix_tree_.root_, suffix_tree_.dummy_, 0, 1);
  SuffixTree::Link link;
  ASSERT_NO_THROW(link = suffix_tree_.GetLink(suffix_tree_.root_,
                                              suffix_tree_.sample_[0]));
  ASSERT_TRUE(link.Equals(SuffixTree::Link(suffix_tree_.dummy_, 0, 1)));
  ASSERT_THROW(suffix_tree_.GetLink(suffix_tree_.dummy_, suffix_tree_.sample_[0]),
               std::out_of_range);
  suffix_tree_.nodes_[suffix_tree_.root_].links.erase(suffix_tree_.sample_[0]);
  ASSERT_THROW(suffix_tree_.GetLink(suffix_tree_.root_, suffix_tree_.sample_[0]),
               std::out_of_range);
}

TEST_F(SuffixTreeTest, CanonicalizeNodeReferenceSimpleTest) {
  suffix_tree_.nodes_.clear();
  suffix_tree_.nodes_.push_back(SuffixTree::Node());
  suffix_tree_.nodes_.push_back(SuffixTree::Node());
  suffix_tree_.nodes_.push_back(SuffixTree::Node());

  suffix_tree_.sample_ = "abc";
  std::string old_sample = suffix_tree_.sample_;
  suffix_tree_.sample_ += suffix_tree_.sample_;

  suffix_tree_.LinkNodes(0, 1, 0, old_sample.size());
  suffix_tree_.LinkNodes(1, 2, 0, old_sample.size());

  SuffixTree::NodeReference node_reference(0, 0, old_sample.size() / 2);
  suffix_tree_.CanonicalizeNodeReference(&node_reference);
  ASSERT_TRUE(node_reference.Equals(SuffixTree::NodeReference(0, 0, old_sample.size() / 2)));

  node_reference = SuffixTree::NodeReference(0, 0, old_sample.size());
  suffix_tree_.CanonicalizeNodeReference(&node_reference);
  SuffixTree::NodeReference correct_node_reference(1, old_sample.size(), old_sample.size());
  ASSERT_TRUE(node_reference.Equals(correct_node_reference));

  node_reference = SuffixTree::NodeReference(0, 0, 3 * old_sample.size() / 2);
  suffix_tree_.CanonicalizeNodeReference(&node_reference);
  correct_node_reference = SuffixTree::NodeReference(1, old_sample.size(),
                                                     3 * old_sample.size() / 2);
  ASSERT_TRUE(node_reference.Equals(correct_node_reference));
}

TEST_F(SuffixTreeTest, CanonicalizeNodeReferenceNormalTest) {
  suffix_tree_.nodes_.clear();
  std::string old_sample = "abacaabc";
  suffix_tree_.sample_ = "";
  int node_quantity = 50;
  suffix_tree_.nodes_.push_back(SuffixTree::Node());
  for (int node_index = 1; node_index < node_quantity; ++node_index) {
    suffix_tree_.nodes_.push_back(SuffixTree::Node());
    suffix_tree_.sample_ += old_sample;
    suffix_tree_.LinkNodes(node_index - 1, node_index, 0, old_sample.size());
  }

  size_t string_end_index;
  for (string_end_index = 0; string_end_index <= suffix_tree_.sample_.size(); ++string_end_index) {
    SuffixTree::NodeReference node_reference(0, 0, string_end_index);
    suffix_tree_.CanonicalizeNodeReference(&node_reference);
    SuffixTree::NodeReference correct_node_reference(string_end_index / old_sample.size(),
                                                     node_reference.closest_ancestor *
                                                     old_sample.size(),
                                                     string_end_index);
    ASSERT_TRUE(node_reference.Equals(correct_node_reference));
  }
}

TEST_F(SuffixTreeTest, TestAndSplitTest) {
  suffix_tree_.nodes_.clear();
  suffix_tree_.nodes_.push_back(SuffixTree::Node());
  suffix_tree_.nodes_.push_back(SuffixTree::Node());

  std::string  first_sample_part = "aaaaabbbbb";
  std::string second_sample_part = "aaaaaabbbb";
  suffix_tree_.sample_ = first_sample_part + second_sample_part;
  suffix_tree_.LinkNodes(0, 1, first_sample_part.size(), suffix_tree_.sample_.size());

  // explicit node, link exists
  int old_nodes_size = suffix_tree_.nodes_.size();
  SuffixTree::NodeReference node_reference(0, 0, 0);
  SuffixTree::TestAndSplitResult test_split_result;
  test_split_result = suffix_tree_.TestAndSplit(node_reference);
  ASSERT_TRUE(test_split_result.Equals(SuffixTree::TestAndSplitResult(true, 0)));
  ASSERT_EQ(old_nodes_size, suffix_tree_.nodes_.size());

  // explicit node, link doesn't exist
  node_reference.sample_start_index = first_sample_part.size() / 2;
  node_reference.sample_end_index = first_sample_part.size() / 2;
  test_split_result = suffix_tree_.TestAndSplit(node_reference);
  ASSERT_TRUE(test_split_result.Equals(SuffixTree::TestAndSplitResult(false, 0)));
  ASSERT_EQ(old_nodes_size, suffix_tree_.nodes_.size());

  // implicit node, link exists
  node_reference.sample_start_index = 0;
  node_reference.sample_end_index = first_sample_part.size() / 2 - 1;
  test_split_result = suffix_tree_.TestAndSplit(node_reference);
  ASSERT_TRUE(test_split_result.Equals(SuffixTree::TestAndSplitResult(true, 0)));
  ASSERT_EQ(old_nodes_size, suffix_tree_.nodes_.size());

  // implicit node, link doesn't exist
  node_reference.sample_start_index = 0;
  node_reference.sample_end_index = first_sample_part.size() / 2;
  test_split_result = suffix_tree_.TestAndSplit(node_reference);
  ASSERT_TRUE(test_split_result.Equals(SuffixTree::TestAndSplitResult(false, old_nodes_size)));
  ASSERT_EQ(suffix_tree_.nodes_.size() - old_nodes_size, 1);  // one node created because of split
  SuffixTree::Link first_link_part, second_link_part;
  char last_first_part_letter = suffix_tree_.sample_[first_sample_part.size()];
  ASSERT_NO_THROW(first_link_part = suffix_tree_.nodes_[0].links.at(last_first_part_letter));
  ASSERT_EQ(first_link_part.target_node_index, old_nodes_size);
  int sample_split_index = (suffix_tree_.sample_.size() + first_sample_part.size()) / 2;
  char split_letter = suffix_tree_.sample_[sample_split_index];
  ASSERT_NO_THROW(second_link_part = suffix_tree_.nodes_.back().links.at(split_letter));
  ASSERT_EQ(second_link_part.target_node_index, 1);
  ASSERT_EQ(first_link_part.sample_start_index, first_sample_part.size());
  ASSERT_EQ(second_link_part.sample_end_index, suffix_tree_.sample_.size());
  ASSERT_EQ(first_link_part.sample_end_index, second_link_part.sample_start_index);
}

TEST_F(SuffixTreeTest, AddNextLetterTestZero) {
  suffix_tree_.AppendSample("a");
  int old_nodes_size = suffix_tree_.nodes_.size();
  suffix_tree_.sample_ += "a";
  SuffixTree::NodeReference node_reference = suffix_tree_.AddNextLetter(suffix_tree_.active_point_);
  ASSERT_TRUE(node_reference.Equals(SuffixTree::NodeReference(suffix_tree_.root_, 1, 1)));
  ASSERT_EQ(suffix_tree_.nodes_.size(), old_nodes_size);  // no new nodes created
  CheckCorrectDummy();
  CheckCorrectRoot();
}

TEST_F(SuffixTreeTest, AddNextLetterTestOne) {
  int old_nodes_size = suffix_tree_.nodes_.size();
  suffix_tree_.sample_ = "a";
  suffix_tree_.InitDummy(0, suffix_tree_.sample_.size());
  SuffixTree::NodeReference node_reference = suffix_tree_.AddNextLetter(suffix_tree_.active_point_);
  ASSERT_TRUE(node_reference.Equals(SuffixTree::NodeReference(suffix_tree_.dummy_, 0, 0)));
  ASSERT_EQ(suffix_tree_.nodes_.size() - old_nodes_size, 1);  // one node was created
  CheckCorrectDummy();
  CheckCorrectRoot();
}

TEST_F(SuffixTreeTest, AddNextLetterTestTwo) {
  suffix_tree_.AppendSample("aa");
  int old_nodes_size = suffix_tree_.nodes_.size();
  suffix_tree_.sample_ += "b";
  suffix_tree_.InitDummy(0, suffix_tree_.sample_.size());
  SuffixTree::NodeReference node_reference = suffix_tree_.AddNextLetter(suffix_tree_.active_point_);
  ASSERT_TRUE(node_reference.Equals(SuffixTree::NodeReference(suffix_tree_.dummy_, 2, 2)));
  ASSERT_EQ(suffix_tree_.nodes_.size() - old_nodes_size, 3);  // 1 split node + 2 inf-branch node
  CheckCorrectDummy();
  CheckCorrectRoot();
}

TEST_F(SuffixTreeTest, UpdateNonExistingCharTest) {
  for (size_t letter_index = 0; letter_index < suffix_tree_.sample_.size(); ++letter_index) {
    ASSERT_LT(suffix_tree_.non_existing_char_, suffix_tree_.sample_[letter_index]);
  }
}

TEST_F(SuffixTreeTest, AppendSampleTest) {
  suffix_tree_.AppendSample("banana$");
  CheckBananaTree();
}

TEST_F(SuffixTreeTest, GetLinkIteratorTest) {
  suffix_tree_.AppendSample("abc");
  SuffixTree::LinkMapConstIterator link_map_it;
  link_map_it = suffix_tree_.GetLinkIterator(suffix_tree_.dummy_, 'a');
  ASSERT_NE(link_map_it, suffix_tree_.nodes_[suffix_tree_.dummy_].links.end());
  ASSERT_TRUE(link_map_it->second.Equals(suffix_tree_.nodes_[suffix_tree_.dummy_].links['a']));

  link_map_it = suffix_tree_.GetLinkIterator(suffix_tree_.dummy_, 'b');
  ASSERT_NE(link_map_it, suffix_tree_.nodes_[suffix_tree_.dummy_].links.end());
  ASSERT_TRUE(link_map_it->second.Equals(suffix_tree_.nodes_[suffix_tree_.dummy_].links['b']));

  link_map_it = suffix_tree_.GetLinkIterator(suffix_tree_.dummy_, 'c');
  ASSERT_NE(link_map_it, suffix_tree_.nodes_[suffix_tree_.dummy_].links.end());
  ASSERT_TRUE(link_map_it->second.Equals(suffix_tree_.nodes_[suffix_tree_.dummy_].links['c']));

  link_map_it = suffix_tree_.GetLinkIterator(suffix_tree_.dummy_, suffix_tree_.non_existing_char_);
  ASSERT_EQ(link_map_it, suffix_tree_.nodes_[suffix_tree_.dummy_].links.end());
}

TEST_F(SuffixTreeTest, IsLeafTest) {
  suffix_tree_.AppendSample("banana$");
  ASSERT_FALSE(suffix_tree_.IsLeaf(0));
  ASSERT_FALSE(suffix_tree_.IsLeaf(1));
  ASSERT_TRUE(suffix_tree_.IsLeaf(2));
  ASSERT_TRUE(suffix_tree_.IsLeaf(3));
  ASSERT_TRUE(suffix_tree_.IsLeaf(4));
  ASSERT_FALSE(suffix_tree_.IsLeaf(5));
  ASSERT_TRUE(suffix_tree_.IsLeaf(6));
  ASSERT_FALSE(suffix_tree_.IsLeaf(7));
  ASSERT_TRUE(suffix_tree_.IsLeaf(8));
  ASSERT_FALSE(suffix_tree_.IsLeaf(9));
  ASSERT_TRUE(suffix_tree_.IsLeaf(10));
  ASSERT_TRUE(suffix_tree_.IsLeaf(11));
}

TEST_F(SuffixTreeTest, sampleTest) {
  std::string sample = "sampletestbananaabcdefghijklmnopqrstuvwxyz";
  suffix_tree_.AppendSample(sample);
  ASSERT_EQ(suffix_tree_.sample(), sample);
}

TEST_F(SuffixTreeTest, SizeTest) {
  size_t old_suffix_tree_size = suffix_tree_.Size();
  for (int node_index = 0; node_index < 100; ++node_index) {
    suffix_tree_.nodes_.push_back(suffix_tree_.nodes_.front());
    ASSERT_EQ(suffix_tree_.Size(), old_suffix_tree_size + node_index + 1);
  }
}

#define UNUSED_VISITOR_METHOD_ARGUMENT(expr) (void)(expr)
class LexicalDFSTraversalVisitor {
 public:
  typedef SuffixTree::LinkMapConstIterator LinkMapConstIterator;
  explicit LexicalDFSTraversalVisitor(const SuffixTree& suffix_tree)
    : suffix_tree_(suffix_tree), traversal_() {}

  void InitVisitor() {}

  void DiscoverNode(const SuffixTree::Link& in_link) {
    traversal_.push_back(in_link.target_node_index);
  }

  void ReturnToNode(const SuffixTree::Link& return_link, const SuffixTree::Link& in_link) {
    UNUSED_VISITOR_METHOD_ARGUMENT(return_link);
    UNUSED_VISITOR_METHOD_ARGUMENT(in_link);
  }

  void ExamineEdge(const SuffixTree::Link& link) {
    UNUSED_VISITOR_METHOD_ARGUMENT(link);
  }

  LinkMapConstIterator ChooseNextNeighbour(int active_node,
                                           const LinkMapConstIterator& link_map_begin_it,
                                           const LinkMapConstIterator& link_map_next_letter_it,
                                           const LinkMapConstIterator& link_map_end_it) {
    UNUSED_VISITOR_METHOD_ARGUMENT(active_node);
    UNUSED_VISITOR_METHOD_ARGUMENT(link_map_begin_it);
    UNUSED_VISITOR_METHOD_ARGUMENT(link_map_end_it);
    return link_map_next_letter_it;
  }

  void FinishNode(const SuffixTree::Link& in_link) {
    UNUSED_VISITOR_METHOD_ARGUMENT(in_link);
  }

  const std::vector<int>& traversal() {
    return traversal_;
  }

 private:
  const SuffixTree& suffix_tree_;
  std::vector<int> traversal_;
};
#undef UNUSED_VISITOR_METHOD_ARGUMENT

TEST_F(SuffixTreeTest, DepthFirstSearchTraversalTest) {
  suffix_tree_.AppendSample("banana$");
  std::vector<int> correct_traversal{1, 11, 9, 10, 5, 6, 3, 2, 7, 8, 4};
  LexicalDFSTraversalVisitor visitor(suffix_tree_);
  suffix_tree_.DepthFirstSearchTraversal<LexicalDFSTraversalVisitor>(&visitor);
  ASSERT_EQ(correct_traversal.size(), visitor.traversal().size());
  for (size_t node_index = 0; node_index < correct_traversal.size(); ++node_index) {
    ASSERT_EQ(correct_traversal[node_index], visitor.traversal()[node_index]);
  }
}

#endif  // TEST_suffix_tree_H
