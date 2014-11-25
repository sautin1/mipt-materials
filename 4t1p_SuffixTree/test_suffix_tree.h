#ifndef TEST_SUFFIX_TREE_H
#define TEST_SUFFIX_TREE_H

#include <gtest/gtest.h>
#include <algorithm>
#include "suffix_tree.h"

class SuffixTreeTest : public ::testing::Test {
protected:

	SuffixTreeTest()
		: suffix_tree("abacaba") {}

	virtual void SetUp() {
		// will be called right before each test
	}

	virtual void TearDown() {
		// will be called right after each test
	}

	SuffixTree suffix_tree;
};

int start_testing(int argc, char** argv) {
	::testing::InitGoogleTest(&argc, argv);
	return RUN_ALL_TESTS();
}

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
	for (size_t sample_start_index = 0; sample_start_index < suffix_tree.sample_.size(); ++sample_start_index) {
		suffix_tree.InitDummy(sample_start_index, sample_start_index + 1);
		ASSERT_EQ(suffix_tree.nodes_[suffix_tree.dummy_].links.size(), sample_start_index + 1);
		std::map<char, SuffixTree::Link>::const_iterator it;
		it = suffix_tree.nodes_[suffix_tree.dummy_].links.find(suffix_tree.sample_[sample_start_index]);
		ASSERT_NE(it, suffix_tree.nodes_[suffix_tree.dummy_].links.end());
	}
	// expected not to overwrite links
	size_t old_link_quantity = suffix_tree.nodes_[suffix_tree.dummy_].links.size();
	suffix_tree.sample_ += suffix_tree.sample_;
	for (size_t sample_start_index = old_link_quantity; sample_start_index < suffix_tree.sample_.size(); ++sample_start_index) {
		SuffixTree::Link old_link = suffix_tree.nodes_[suffix_tree.dummy_].links[suffix_tree.sample_[sample_start_index]];
		suffix_tree.InitDummy(sample_start_index, sample_start_index + 1);
		SuffixTree::Link new_link = suffix_tree.nodes_[suffix_tree.dummy_].links[suffix_tree.sample_[sample_start_index]];
		ASSERT_EQ(old_link, new_link);

		ASSERT_EQ(suffix_tree.nodes_[suffix_tree.dummy_].links.size(), old_link_quantity);
		std::map<char, SuffixTree::Link>::const_iterator it;
		it = suffix_tree.nodes_[suffix_tree.dummy_].links.find(suffix_tree.sample_[sample_start_index]);
		ASSERT_NE(it, suffix_tree.nodes_[suffix_tree.dummy_].links.end());
	}

	// add the whole string
	std::reverse(suffix_tree.sample_.begin(), suffix_tree.sample_.end());
	suffix_tree.sample_ += suffix_tree.sample_;
	suffix_tree.InitDummy(0, suffix_tree.sample_.size());
	ASSERT_EQ(suffix_tree.nodes_[suffix_tree.dummy_].links.size(), old_link_quantity);
}

TEST_F(SuffixTreeTest, InitLetterSetTest) {
	suffix_tree.letter_set_.clear();
	suffix_tree.sample_ = "abcdefghijklmnopqrstuvwxyz";
	// expected to add letters
	for (size_t sample_start_index = 0; sample_start_index < suffix_tree.sample_.size(); ++sample_start_index) {
		suffix_tree.InitLetterSet(sample_start_index, sample_start_index + 1);
		ASSERT_EQ(suffix_tree.letter_set_.size(), sample_start_index + 1);
		std::set<char>::const_iterator it;
		it = suffix_tree.letter_set_.find(suffix_tree.sample_[sample_start_index]);
		ASSERT_NE(it, suffix_tree.letter_set_.end());
	}

	// add the whole string
	size_t old_letter_set_size = suffix_tree.letter_set_.size();
	suffix_tree.letter_set_.clear();
	suffix_tree.sample_ += suffix_tree.sample_;
	suffix_tree.InitLetterSet(0, suffix_tree.sample_.size());
	ASSERT_EQ(suffix_tree.letter_set_.size(), old_letter_set_size);
	// check that every letter of the sample is in the set of letters
	for (size_t letter_index = 0; letter_index < suffix_tree.sample_.size(); ++letter_index) {
		std::set<char>::const_iterator it;
		it = suffix_tree.letter_set_.find(suffix_tree.sample_[letter_index]);
		ASSERT_NE(it, suffix_tree.letter_set_.end());
	}
}

TEST_F(SuffixTreeTest, SizeTest) {
	ASSERT_NE(suffix_tree.Size(), 0);
	for (int test_index = 0; test_index < 100; ++test_index) {
		suffix_tree.nodes_.push_back(suffix_tree.nodes_.front());
		ASSERT_EQ(suffix_tree.Size(), suffix_tree.nodes_.size());
	}
}

TEST_F(SuffixTreeTest, InitTreeTest) {
	suffix_tree.InitTree();
	ASSERT_LT(suffix_tree.dummy_, suffix_tree.nodes_.size());
	ASSERT_LT(suffix_tree.root_, suffix_tree.nodes_.size());
	ASSERT_EQ(suffix_tree.nodes_[suffix_tree.dummy_].suffix_link_node_index, -1);
	ASSERT_EQ(suffix_tree.nodes_[suffix_tree.root_].suffix_link_node_index, suffix_tree.dummy_);
	ASSERT_EQ(suffix_tree.active_point_.closest_ancestor, suffix_tree.root_);
	ASSERT_EQ(suffix_tree.active_point_.sample_start_index, 0);
	ASSERT_EQ(suffix_tree.active_point_.sample_end_index, 0);
}

TEST_F(SuffixTreeTest, LinkNodesTest) {
	suffix_tree.nodes_[suffix_tree.root_].links.clear();
	suffix_tree.LinkNodes(suffix_tree.root_, suffix_tree.dummy_, 0, 1);
	ASSERT_EQ(suffix_tree.nodes_[suffix_tree.root_].links.size(), 1);
	std::map<char, SuffixTree::Link>::const_iterator it;
	it = suffix_tree.nodes_[suffix_tree.root_].links.find(suffix_tree.sample_[0]);
	ASSERT_NE(it, suffix_tree.nodes_[suffix_tree.root_].links.end());
	ASSERT_EQ(it->second.target_node_index, suffix_tree.dummy_);
	ASSERT_EQ(it->second.sample_start_index, 0);
	ASSERT_EQ(it->second.sample_end_index, 1);
}

TEST_F(SuffixTreeTest, HasLinkTest) {
	for (size_t letter_index = 0; letter_index < suffix_tree.sample_.size(); ++letter_index) {
		ASSERT_TRUE(suffix_tree.HasLink(suffix_tree.dummy_, suffix_tree.sample_[letter_index]));
	}

	int last_node_index = suffix_tree.nodes_.size() - 1;
	suffix_tree.nodes_[last_node_index].links.clear();
	suffix_tree.nodes_[suffix_tree.dummy_].links.clear();
	suffix_tree.LinkNodes(last_node_index, suffix_tree.dummy_, 0, 1);
	ASSERT_TRUE(suffix_tree.HasLink(last_node_index, suffix_tree.sample_[0]));
	ASSERT_FALSE(suffix_tree.HasLink(suffix_tree.dummy_, suffix_tree.sample_[0]));
	suffix_tree.nodes_[last_node_index].links.erase(suffix_tree.sample_[0]);
	ASSERT_FALSE(suffix_tree.HasLink(last_node_index, suffix_tree.sample_[0]));
}

TEST_F(SuffixTreeTest, GetLinkTest) {
	SuffixTree::Link link;
	for (size_t letter_index = 0; letter_index < suffix_tree.sample_.size(); ++letter_index) {
		ASSERT_NO_THROW(link = suffix_tree.GetLink(suffix_tree.dummy_, suffix_tree.sample_[letter_index]));
		ASSERT_EQ(link.target_node_index, suffix_tree.root_);
		ASSERT_LT(link.sample_start_index, link.sample_end_index);
		ASSERT_EQ(suffix_tree.sample_[link.sample_start_index], suffix_tree.sample_[letter_index]);
	}

	int last_node_index = suffix_tree.nodes_.size() - 1;
	suffix_tree.nodes_[last_node_index].links.clear();
	suffix_tree.nodes_[suffix_tree.dummy_].links.clear();
	ASSERT_THROW(suffix_tree.GetLink(last_node_index, suffix_tree.sample_[0]), std::out_of_range);
	suffix_tree.LinkNodes(last_node_index, suffix_tree.dummy_, 0, 1);
	ASSERT_NO_THROW(link = suffix_tree.GetLink(last_node_index, suffix_tree.sample_[0]));
	ASSERT_EQ(link.target_node_index, suffix_tree.dummy_);
	ASSERT_EQ(link.sample_start_index, 0);
	ASSERT_EQ(link.sample_end_index, 1);
	ASSERT_THROW(suffix_tree.GetLink(suffix_tree.dummy_, suffix_tree.sample_[0]), std::out_of_range);
	suffix_tree.nodes_[last_node_index].links.erase(suffix_tree.sample_[0]);
	ASSERT_THROW(suffix_tree.GetLink(last_node_index, suffix_tree.sample_[0]), std::out_of_range);
}

TEST_F(SuffixTreeTest, CanonicalizeNodeReferenceSimpleTest) {
	suffix_tree.nodes_.clear();
	suffix_tree.sample_.pop_back(); // erase non-existing symbol
	std::string old_sample = suffix_tree.sample_;
	suffix_tree.sample_ += suffix_tree.sample_;
	suffix_tree.nodes_.push_back(SuffixTree::Node());
	suffix_tree.nodes_.push_back(SuffixTree::Node());
	suffix_tree.nodes_.push_back(SuffixTree::Node());
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
	suffix_tree.sample_.pop_back(); // erase non-existing symbol
	std::string old_sample = suffix_tree.sample_;
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

}

/*
	TestAndSplitResult TestAndSplit(const NodeReference& node_reference);

	NodeReference AddNextLetter(NodeReference active_point);
	void BuildTree();

*/

/*void TestSuffixTree::TestAddNextLetter() const {
	std::cout << "TestCreateNode: ";
	SuffixTree suffix_tree("a");
	int suffix_tree_size = suffix_tree.Size();
	int created_node_index = suffix_tree.CreateNode();
	AssertEquals(suffix_tree.Size(), suffix_tree_size + 1);
	AssertEquals(created_node_index, suffix_tree_size);
	AssertEquals(suffix_tree.nodes_[created_node_index].suffix_link_node_index, -1);
	std::cout << "passed!";
}*/



#endif // TEST_SUFFIX_TREE_H
