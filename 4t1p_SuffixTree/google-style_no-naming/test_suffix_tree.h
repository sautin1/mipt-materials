#ifndef TEST_SUFFIX_TREE_H
#define TEST_SUFFIX_TREE_H
#define UNUSED(expr) (void)(expr)

#include <gtest/gtest.h>
#include <algorithm>
#include "suffix_tree.h"

class SuffixTreeTest : public ::testing::Test {
protected:
	void CheckBananaTree() {
		ASSERT_EQ(suffix_tree.nodes_.size(), 12);

		ASSERT_EQ(suffix_tree.dummy_, 0);
		ASSERT_EQ(suffix_tree.nodes_[0].links.size(), 4);
		ASSERT_EQ(suffix_tree.nodes_[0].suffix_link_node_index, -1);
		for (size_t letter_index = 0; letter_index < suffix_tree.sample_.size(); ++letter_index) {
			ASSERT_NO_THROW(suffix_tree.nodes_[0].links.at(suffix_tree.sample_[letter_index]));
		}

		ASSERT_EQ(suffix_tree.root_, 1);
		ASSERT_EQ(suffix_tree.nodes_[1].links.size(), 4);
		ASSERT_EQ(suffix_tree.nodes_[1].suffix_link_node_index, 0);
		SuffixTree::Link link;
		ASSERT_NO_THROW(link = suffix_tree.nodes_[1].links.at('b'));
		ASSERT_EQ(link, SuffixTree::Link(2, 0, suffix_tree.INF));
		ASSERT_NO_THROW(link = suffix_tree.nodes_[1].links.at('a'));
		ASSERT_EQ(link, SuffixTree::Link(9, 1, 2));
		ASSERT_NO_THROW(link = suffix_tree.nodes_[1].links.at('n'));
		ASSERT_EQ(link, SuffixTree::Link(7, 2, 4));
		ASSERT_NO_THROW(link = suffix_tree.nodes_[1].links.at('$'));
		ASSERT_EQ(link, SuffixTree::Link(11, 6, suffix_tree.INF));

		ASSERT_EQ(suffix_tree.nodes_[2].links.size(), 0);
		ASSERT_EQ(suffix_tree.nodes_[2].suffix_link_node_index, -1);

		ASSERT_EQ(suffix_tree.nodes_[3].links.size(), 0);
		ASSERT_EQ(suffix_tree.nodes_[3].suffix_link_node_index, -1);

		ASSERT_EQ(suffix_tree.nodes_[4].links.size(), 0);
		ASSERT_EQ(suffix_tree.nodes_[4].suffix_link_node_index, -1);

		ASSERT_EQ(suffix_tree.nodes_[5].links.size(), 2);
		ASSERT_EQ(suffix_tree.nodes_[5].suffix_link_node_index, 7);
		ASSERT_NO_THROW(link = suffix_tree.nodes_[5].links.at('$'));
		ASSERT_EQ(link, SuffixTree::Link(6, 6, suffix_tree.INF));
		ASSERT_NO_THROW(link = suffix_tree.nodes_[5].links.at('n'));
		ASSERT_EQ(link, SuffixTree::Link(3, 4, suffix_tree.INF));

		ASSERT_EQ(suffix_tree.nodes_[6].links.size(), 0);
		ASSERT_EQ(suffix_tree.nodes_[6].suffix_link_node_index, -1);

		ASSERT_EQ(suffix_tree.nodes_[7].links.size(), 2);
		ASSERT_EQ(suffix_tree.nodes_[7].suffix_link_node_index, 9);
		ASSERT_NO_THROW(link = suffix_tree.nodes_[7].links.at('$'));
		ASSERT_EQ(link, SuffixTree::Link(8, 6, suffix_tree.INF));
		ASSERT_NO_THROW(link = suffix_tree.nodes_[7].links.at('n'));
		ASSERT_EQ(link, SuffixTree::Link(4, 4, suffix_tree.INF));

		ASSERT_EQ(suffix_tree.nodes_[8].links.size(), 0);
		ASSERT_EQ(suffix_tree.nodes_[8].suffix_link_node_index, -1);

		ASSERT_EQ(suffix_tree.nodes_[9].links.size(), 2);
		ASSERT_EQ(suffix_tree.nodes_[9].suffix_link_node_index, 1);
		ASSERT_NO_THROW(link = suffix_tree.nodes_[9].links.at('$'));
		ASSERT_EQ(link, SuffixTree::Link(10, 6, suffix_tree.INF));
		ASSERT_NO_THROW(link = suffix_tree.nodes_[9].links.at('n'));
		ASSERT_EQ(link, SuffixTree::Link(5, 2, 4));

		ASSERT_EQ(suffix_tree.nodes_[10].links.size(), 0);
		ASSERT_EQ(suffix_tree.nodes_[10].suffix_link_node_index, -1);

		ASSERT_EQ(suffix_tree.nodes_[11].links.size(), 0);
		ASSERT_EQ(suffix_tree.nodes_[11].suffix_link_node_index, -1);
	}

	void CheckCorrectDummy() {
		ASSERT_LT(suffix_tree.dummy_, suffix_tree.nodes_.size());
		ASSERT_EQ(suffix_tree.dummy_, 0);
		for (size_t letter_index = 0; letter_index < suffix_tree.sample_.size(); ++letter_index) {
			ASSERT_NO_THROW(suffix_tree.nodes_[suffix_tree.dummy_].links.at(suffix_tree.sample_[letter_index]));
		}
		ASSERT_EQ(suffix_tree.nodes_[0].suffix_link_node_index, -1);
	}

	void CheckCorrectRoot() {
		ASSERT_LT(suffix_tree.root_, suffix_tree.nodes_.size());
		ASSERT_EQ(suffix_tree.root_, 1);
		for (size_t letter_index = 0; letter_index < suffix_tree.sample_.size(); ++letter_index) {
			ASSERT_NO_THROW(suffix_tree.nodes_[suffix_tree.root_].links.at(suffix_tree.sample_[letter_index]));
		}
		ASSERT_EQ(suffix_tree.nodes_[1].suffix_link_node_index, 0);
	}

	void CheckLetterSet() {
		for (size_t letter_index = 0; letter_index < suffix_tree.sample_.size(); ++letter_index) {
			std::set<char>::const_iterator it;
			it = suffix_tree.letter_set_.find(suffix_tree.sample_[letter_index]);
			ASSERT_NE(it, suffix_tree.letter_set_.end());
		}
	}

	SuffixTree suffix_tree;
};

TEST_F(SuffixTreeTest, CreateNodeTest) {
	int old_suffix_tree_size = suffix_tree.nodes_.size();
	for (int insert_index = 0; insert_index < 100; ++insert_index) {
		int created_node_index = suffix_tree.CreateNode();
		ASSERT_EQ(created_node_index, old_suffix_tree_size + insert_index);
		ASSERT_EQ(suffix_tree.nodes_.size(), old_suffix_tree_size + insert_index + 1);
		ASSERT_EQ(suffix_tree.nodes_[created_node_index].suffix_link_node_index, -1);
	}
}

TEST_F(SuffixTreeTest, InitDummyTest) {
	suffix_tree.nodes_[suffix_tree.dummy_].links.clear();
	suffix_tree.sample_ = "abcdefghijklmnopqrstuvwxyz";
	// expected to add links
	suffix_tree.InitDummy(0, suffix_tree.sample_.size());
	CheckCorrectDummy();

	// expected not to overwrite links
	size_t old_link_quantity = suffix_tree.nodes_[suffix_tree.dummy_].links.size();
	suffix_tree.sample_ += suffix_tree.sample_;
	for (size_t sample_start_index = old_link_quantity; sample_start_index < suffix_tree.sample_.size(); ++sample_start_index) {
		SuffixTree::Link old_link = suffix_tree.nodes_[suffix_tree.dummy_].links[suffix_tree.sample_[sample_start_index]];
		suffix_tree.InitDummy(sample_start_index, sample_start_index + 1);
		SuffixTree::Link new_link;
		ASSERT_NO_THROW(new_link = suffix_tree.nodes_[suffix_tree.dummy_].links.at(suffix_tree.sample_[sample_start_index]));
		ASSERT_EQ(old_link, new_link);
		ASSERT_EQ(suffix_tree.nodes_[suffix_tree.dummy_].links.size(), old_link_quantity);
	}
}

TEST_F(SuffixTreeTest, InitLetterSetTest) {
	suffix_tree.letter_set_.clear();
	suffix_tree.sample_ = "abcdefghijklmnopqrstuvwxyz";
	suffix_tree.InitLetterSet(0, suffix_tree.sample_.size());
	CheckLetterSet();
}

TEST_F(SuffixTreeTest, InitTreeTest) {
	suffix_tree.nodes_.clear();
	suffix_tree.InitTree();
	CheckCorrectDummy();
	CheckCorrectRoot();
	ASSERT_EQ(suffix_tree.active_point_.closest_ancestor, suffix_tree.root_);
	ASSERT_EQ(suffix_tree.active_point_.sample_start_index, 0);
	ASSERT_EQ(suffix_tree.active_point_.sample_end_index, 0);
}

TEST_F(SuffixTreeTest, LinkNodesTest) {
	suffix_tree.nodes_[suffix_tree.root_].links.clear();
	suffix_tree.LinkNodes(suffix_tree.root_, suffix_tree.dummy_, 0, 1);
	ASSERT_EQ(suffix_tree.nodes_[suffix_tree.root_].links.size(), 1);
	SuffixTree::Link link;
	ASSERT_NO_THROW(link = suffix_tree.nodes_[suffix_tree.root_].links.at(suffix_tree.sample_[0]));
	ASSERT_EQ(link.target_node_index, suffix_tree.dummy_);
	ASSERT_EQ(link.sample_start_index, 0);
	ASSERT_EQ(link.sample_end_index, 1);
}

TEST_F(SuffixTreeTest, HasLinkTest) {
	suffix_tree.nodes_[suffix_tree.dummy_].links.clear();
	suffix_tree.nodes_[suffix_tree.root_].links.clear();
	suffix_tree.LinkNodes(suffix_tree.root_, suffix_tree.dummy_, 0, 1);
	ASSERT_TRUE(suffix_tree.HasLink(suffix_tree.root_, suffix_tree.sample_[0]));
	ASSERT_FALSE(suffix_tree.HasLink(suffix_tree.dummy_, suffix_tree.sample_[0]));
	suffix_tree.nodes_[suffix_tree.root_].links.erase(suffix_tree.sample_[0]);
	ASSERT_FALSE(suffix_tree.HasLink(suffix_tree.root_, suffix_tree.sample_[0]));
}

TEST_F(SuffixTreeTest, GetLinkTest) {
	suffix_tree.nodes_[suffix_tree.root_].links.clear();
	suffix_tree.nodes_[suffix_tree.dummy_].links.clear();
	ASSERT_THROW(suffix_tree.GetLink(suffix_tree.root_, suffix_tree.sample_[0]), std::out_of_range);
	suffix_tree.LinkNodes(suffix_tree.root_, suffix_tree.dummy_, 0, 1);
	SuffixTree::Link link;
	ASSERT_NO_THROW(link = suffix_tree.GetLink(suffix_tree.root_, suffix_tree.sample_[0]));
	ASSERT_EQ(link.target_node_index, suffix_tree.dummy_);
	ASSERT_EQ(link.sample_start_index, 0);
	ASSERT_EQ(link.sample_end_index, 1);
	ASSERT_THROW(suffix_tree.GetLink(suffix_tree.dummy_, suffix_tree.sample_[0]), std::out_of_range);
	suffix_tree.nodes_[suffix_tree.root_].links.erase(suffix_tree.sample_[0]);
	ASSERT_THROW(suffix_tree.GetLink(suffix_tree.root_, suffix_tree.sample_[0]), std::out_of_range);
}

TEST_F(SuffixTreeTest, CanonicalizeNodeReferenceSimpleTest) {
	suffix_tree.nodes_.clear();
	suffix_tree.nodes_.push_back(SuffixTree::Node());
	suffix_tree.nodes_.push_back(SuffixTree::Node());
	suffix_tree.nodes_.push_back(SuffixTree::Node());

	suffix_tree.sample_ = "abc";
	std::string old_sample = suffix_tree.sample_;
	suffix_tree.sample_ += suffix_tree.sample_;

	suffix_tree.LinkNodes(0, 1, 0, old_sample.size());
	suffix_tree.LinkNodes(1, 2, 0, old_sample.size());

	SuffixTree::NodeReference node_reference(0, 0, old_sample.size() / 2);
	suffix_tree.CanonicalizeNodeReference(&node_reference);
	ASSERT_EQ(SuffixTree::NodeReference(0, 0, old_sample.size() / 2), node_reference);

	node_reference = SuffixTree::NodeReference(0, 0, old_sample.size());
	suffix_tree.CanonicalizeNodeReference(&node_reference);
	ASSERT_EQ(SuffixTree::NodeReference(1, old_sample.size(), old_sample.size()), node_reference);

	node_reference = SuffixTree::NodeReference(0, 0, 3 * old_sample.size() / 2);
	suffix_tree.CanonicalizeNodeReference(&node_reference);
	ASSERT_EQ(SuffixTree::NodeReference(1, old_sample.size(), 3 * old_sample.size() / 2), node_reference);
}

TEST_F(SuffixTreeTest, CanonicalizeNodeReferenceNormalTest) {
	suffix_tree.nodes_.clear();
	std::string old_sample = "abacaabc";
	suffix_tree.sample_ = "";
	int node_quantity = 50;
	suffix_tree.nodes_.push_back(SuffixTree::Node());
	for (int node_index = 1; node_index < node_quantity; ++node_index) {
		suffix_tree.nodes_.push_back(SuffixTree::Node());
		suffix_tree.sample_ += old_sample;
		suffix_tree.LinkNodes(node_index - 1, node_index, 0, old_sample.size());
	}

	for (size_t string_end_index = 0; string_end_index <= suffix_tree.sample_.size(); ++string_end_index) {
		SuffixTree::NodeReference node_reference(0, 0, string_end_index);
		suffix_tree.CanonicalizeNodeReference(&node_reference);
		ASSERT_EQ(node_reference.closest_ancestor, string_end_index / old_sample.size());
		ASSERT_EQ(node_reference.sample_start_index, node_reference.closest_ancestor * old_sample.size());
		ASSERT_EQ(node_reference.sample_end_index, string_end_index);
	}
}

TEST_F(SuffixTreeTest, TestAndSplitTest) {
	suffix_tree.nodes_.clear();
	suffix_tree.nodes_.push_back(SuffixTree::Node());
	suffix_tree.nodes_.push_back(SuffixTree::Node());

	std::string  first_sample_part = "aaaaabbbbb";
	std::string second_sample_part = "aaaaaabbbb";
	suffix_tree.sample_ = first_sample_part + second_sample_part;
	suffix_tree.LinkNodes(0, 1, first_sample_part.size(), suffix_tree.sample_.size());

	// explicit node, link exists
	int old_nodes_size = suffix_tree.nodes_.size();
	SuffixTree::NodeReference node_reference(0, 0, 0);
	SuffixTree::TestAndSplitResult test_split_result;
	test_split_result = suffix_tree.TestAndSplit(node_reference);
	ASSERT_EQ(test_split_result, SuffixTree::TestAndSplitResult(true, 0));
	ASSERT_EQ(old_nodes_size, suffix_tree.nodes_.size());

	// explicit node, link doesn't exist
	node_reference.sample_start_index = first_sample_part.size() / 2;
	node_reference.sample_end_index = first_sample_part.size() / 2;
	test_split_result = suffix_tree.TestAndSplit(node_reference);
	ASSERT_EQ(test_split_result, SuffixTree::TestAndSplitResult(false, 0));
	ASSERT_EQ(old_nodes_size, suffix_tree.nodes_.size());

	// implicit node, link exists
	node_reference.sample_start_index = 0;
	node_reference.sample_end_index = first_sample_part.size() / 2 - 1;
	test_split_result = suffix_tree.TestAndSplit(node_reference);
	ASSERT_EQ(test_split_result, SuffixTree::TestAndSplitResult(true, 0));
	ASSERT_EQ(old_nodes_size, suffix_tree.nodes_.size());

	// implicit node, link doesn't exist
	node_reference.sample_start_index = 0;
	node_reference.sample_end_index = first_sample_part.size() / 2;
	test_split_result = suffix_tree.TestAndSplit(node_reference);
	ASSERT_EQ(test_split_result, SuffixTree::TestAndSplitResult(false, old_nodes_size));
	ASSERT_EQ(suffix_tree.nodes_.size() - old_nodes_size, 1); // one node was created because of split
	SuffixTree::Link first_link_part, second_link_part;
	ASSERT_NO_THROW(first_link_part = suffix_tree.nodes_[0].links.at(suffix_tree.sample_[first_sample_part.size()]));
	ASSERT_EQ(first_link_part.target_node_index, old_nodes_size);
	int sample_split_index = (suffix_tree.sample_.size() + first_sample_part.size()) / 2;
	ASSERT_NO_THROW(second_link_part = suffix_tree.nodes_.back().links.at(suffix_tree.sample_[sample_split_index]));
	ASSERT_EQ(second_link_part.target_node_index, 1);
	ASSERT_EQ(first_link_part.sample_start_index, first_sample_part.size());
	ASSERT_EQ(second_link_part.sample_end_index, suffix_tree.sample_.size());
	ASSERT_EQ(first_link_part.sample_end_index, second_link_part.sample_start_index);
}

TEST_F(SuffixTreeTest, AddNextLetterTestZero) {
	suffix_tree.AppendSample("a");
	int old_nodes_size = suffix_tree.nodes_.size();
	suffix_tree.sample_ += "a";
	SuffixTree::NodeReference node_reference = suffix_tree.AddNextLetter(suffix_tree.active_point_);
	ASSERT_EQ(node_reference, SuffixTree::NodeReference(suffix_tree.root_, 1, 1));
	ASSERT_EQ(suffix_tree.nodes_.size(), old_nodes_size); // no new nodes created
	CheckCorrectDummy();
	CheckCorrectRoot();
}

TEST_F(SuffixTreeTest, AddNextLetterTestOne) {
	int old_nodes_size = suffix_tree.nodes_.size();
	suffix_tree.sample_ = "a";
	suffix_tree.InitDummy(0, suffix_tree.sample_.size());
	SuffixTree::NodeReference node_reference = suffix_tree.AddNextLetter(suffix_tree.active_point_);
	ASSERT_EQ(node_reference, SuffixTree::NodeReference(suffix_tree.dummy_, 0, 0));
	ASSERT_EQ(suffix_tree.nodes_.size() - old_nodes_size, 1); // one node was created
	CheckCorrectDummy();
	CheckCorrectRoot();
}

TEST_F(SuffixTreeTest, AddNextLetterTestTwo) {
	suffix_tree.AppendSample("aa");
	int old_nodes_size = suffix_tree.nodes_.size();
	suffix_tree.sample_ += "b";
	suffix_tree.InitDummy(0, suffix_tree.sample_.size());
	SuffixTree::NodeReference node_reference = suffix_tree.AddNextLetter(suffix_tree.active_point_);
	ASSERT_EQ(node_reference, SuffixTree::NodeReference(suffix_tree.dummy_, 2, 2));
	ASSERT_EQ(suffix_tree.nodes_.size() - old_nodes_size, 3); // 1 split node + 2 inf-branch node
	CheckCorrectDummy();
	CheckCorrectRoot();
}

TEST_F(SuffixTreeTest, UpdateNonExistingCharTest) {
	for (size_t letter_index = 0; letter_index < suffix_tree.sample_.size(); ++letter_index) {
		ASSERT_LT(suffix_tree.non_existing_char_, suffix_tree.sample_[letter_index]);
	}
}

TEST_F(SuffixTreeTest, AppendSampleTest) {
	suffix_tree.AppendSample("banana$");
	CheckBananaTree();
}

TEST_F(SuffixTreeTest, GetLinkIteratorTest) {
	suffix_tree.AppendSample("abc");
	SuffixTree::LinkMapConstIterator it = suffix_tree.GetLinkIterator(suffix_tree.dummy_, 'a');
	ASSERT_NE(it, suffix_tree.nodes_[suffix_tree.dummy_].links.end());
	ASSERT_EQ(it->second, suffix_tree.nodes_[suffix_tree.dummy_].links['a']);

	it = suffix_tree.GetLinkIterator(suffix_tree.dummy_, 'b');
	ASSERT_NE(it, suffix_tree.nodes_[suffix_tree.dummy_].links.end());
	ASSERT_EQ(it->second, suffix_tree.nodes_[suffix_tree.dummy_].links['b']);

	it = suffix_tree.GetLinkIterator(suffix_tree.dummy_, 'c');
	ASSERT_NE(it, suffix_tree.nodes_[suffix_tree.dummy_].links.end());
	ASSERT_EQ(it->second, suffix_tree.nodes_[suffix_tree.dummy_].links['c']);

	it = suffix_tree.GetLinkIterator(suffix_tree.dummy_, suffix_tree.non_existing_char_);
	ASSERT_EQ(it, suffix_tree.nodes_[suffix_tree.dummy_].links.end());
}

TEST_F(SuffixTreeTest, IsLeafTest) {
	suffix_tree.AppendSample("banana$");
	ASSERT_FALSE(suffix_tree.IsLeaf(0 ));
	ASSERT_FALSE(suffix_tree.IsLeaf(1 ));
	ASSERT_TRUE (suffix_tree.IsLeaf(2 ));
	ASSERT_TRUE (suffix_tree.IsLeaf(3 ));
	ASSERT_TRUE (suffix_tree.IsLeaf(4 ));
	ASSERT_FALSE(suffix_tree.IsLeaf(5 ));
	ASSERT_TRUE (suffix_tree.IsLeaf(6 ));
	ASSERT_FALSE(suffix_tree.IsLeaf(7 ));
	ASSERT_TRUE (suffix_tree.IsLeaf(8 ));
	ASSERT_FALSE(suffix_tree.IsLeaf(9 ));
	ASSERT_TRUE (suffix_tree.IsLeaf(10));
	ASSERT_TRUE (suffix_tree.IsLeaf(11));
}

TEST_F(SuffixTreeTest, sampleTest) {
	std::string sample = "sampletestbananaabcdefghijklmnopqrstuvwxyz";
	suffix_tree.AppendSample(sample);
	ASSERT_EQ(suffix_tree.sample(), sample);
}

TEST_F(SuffixTreeTest, SizeTest) {
	size_t old_suffix_tree_size = suffix_tree.Size();
	for (int node_index = 0; node_index < 100; ++node_index) {
		suffix_tree.nodes_.push_back(suffix_tree.nodes_.front());
		ASSERT_EQ(suffix_tree.Size(), old_suffix_tree_size + node_index + 1);
	}
}

class LexicalDFSTraversalVisitor {
private:
	typedef SuffixTree::LinkMapConstIterator LinkMapConstIterator;
	const SuffixTree& suffix_tree;
public:
	LexicalDFSTraversalVisitor(const SuffixTree& _suffix_tree)
		: suffix_tree(_suffix_tree), traversal() {}

	void InitVisitor() {}

	void DiscoverNode(const SuffixTree::Link& in_link) {
		traversal.push_back(in_link.target_node_index);
	}

	void ReturnToNode(const SuffixTree::Link& return_link, const SuffixTree::Link& in_link) {
		UNUSED(return_link);
		UNUSED(in_link);
	}

	void ExamineEdge(const SuffixTree::Link& link) {
		UNUSED(link);
	}

	LinkMapConstIterator ChooseNextNeighbour(int active_node, const LinkMapConstIterator& link_map_begin_it,
			const LinkMapConstIterator& link_map_next_letter_it, const LinkMapConstIterator& link_map_end_it) {
		UNUSED(active_node);
		UNUSED(link_map_begin_it);
		UNUSED(link_map_end_it);
		return link_map_next_letter_it;
	}

	void FinishNode(const SuffixTree::Link& in_link) {
		UNUSED(in_link);
	}

	std::vector<int> traversal;
};

TEST_F(SuffixTreeTest, DepthFirstSearchTraversalTest) {
	suffix_tree.AppendSample("banana$");
	std::vector<int> correct_traversal {1, 11, 9, 10, 5, 6, 3, 2, 7, 8, 4};
	LexicalDFSTraversalVisitor visitor(suffix_tree);
	suffix_tree.DepthFirstSearchTraversal<LexicalDFSTraversalVisitor>(visitor);
	ASSERT_EQ(correct_traversal.size(), visitor.traversal.size());
	for (size_t node_index = 0; node_index < correct_traversal.size(); ++node_index) {
		ASSERT_EQ(correct_traversal[node_index], visitor.traversal[node_index]);
	}
}

#endif // TEST_SUFFIX_TREE_H
