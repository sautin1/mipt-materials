// Copyright 2014, MIPT
// All rights reserved.
//
// Author: sautin1@yandex.ru (Andrew Sautin)
//
// The Suffix Tree C++ Library Tester
//
// This file defines the public API for working with suffix tree.
//
// For building the suffix tree Ukkonen's algorithm is used.
//
// Method DepthFirstSearchTraversal() is created for users to solve different problems on strings.

#include "suffix_tree.h"

SuffixTree::Link::Link()
	: target_node_index (-1) {}

SuffixTree::Link::Link(int _target_node_index, int _sample_start_index, int _sample_end_index)
	: target_node_index(_target_node_index), sample_start_index(_sample_start_index), sample_end_index(_sample_end_index) {}

bool SuffixTree::Link::Equals(const Link& link) const {
	bool is_equal = target_node_index == link.target_node_index && sample_start_index == link.sample_start_index;
	is_equal = is_equal && sample_end_index == link.sample_end_index;
	return is_equal;
}

SuffixTree::Node::Node()
	: links(), suffix_link_node_index(-1) {}

SuffixTree::NodeReference::NodeReference() {}

SuffixTree::NodeReference::NodeReference(int _closest_ancestor_node, int _sample_start_index, int _sample_end_index)
	: closest_ancestor(_closest_ancestor_node), sample_start_index(_sample_start_index), sample_end_index(_sample_end_index) {}

bool SuffixTree::NodeReference::Equals(const NodeReference& node_reference) const {
	bool is_equal = closest_ancestor == node_reference.closest_ancestor && sample_start_index == node_reference.sample_start_index;
	is_equal = is_equal && sample_end_index == node_reference.sample_end_index;
	return is_equal;
}

SuffixTree::TestAndSplitResult::TestAndSplitResult() {}

SuffixTree::TestAndSplitResult::TestAndSplitResult(bool _reached_endpoint, int _node_index)
	: reached_endpoint(_reached_endpoint), node_index(_node_index) {}

bool SuffixTree::TestAndSplitResult::Equals (const TestAndSplitResult& test_split_result) const {
	return (node_index == test_split_result.node_index) && (reached_endpoint == test_split_result.reached_endpoint);
}

SuffixTree::DepthFirstSearchStackItem::DepthFirstSearchStackItem(const Link& _enter_link, const LinkMapConstIterator& _next_link_letter_it)
	: enter_link(_enter_link), next_link_letter_it(_next_link_letter_it) {}

SuffixTree::SuffixTree()
	: sample_(), non_existing_char_('\1') {
	InitTree();
}

SuffixTree::SuffixTree(const std::string& sample)
	: sample_(), non_existing_char_('\1') {
	InitTree();
	AppendSample(sample + non_existing_char_);
}

void SuffixTree::UpdateNonExistingChar() {
	while (non_existing_char_ <= std::numeric_limits<char>::max()) {
		if (letter_set_.find(non_existing_char_) == letter_set_.end()) {
			break;
		}
		++non_existing_char_;
	}
}

void SuffixTree::AppendSample(const std::string& append_sample) {
	size_t old_sample_size = sample_.size();
	sample_.append(append_sample);
	InitDummy(old_sample_size, sample_.size());
	InitLetterSet(old_sample_size, sample_.size());
	UpdateNonExistingChar();
	for (size_t letter_index = old_sample_size; letter_index < sample_.size(); ++letter_index) {
		active_point_ = AddNextLetter(active_point_);
		++active_point_.sample_end_index;
		CanonicalizeNodeReference(&active_point_);
	}
}

SuffixTree::LinkMapConstIterator SuffixTree::GetLinkIterator(int node_index, char letter) const {
	return nodes_[node_index].links.find(letter);
}

bool SuffixTree::IsLeaf(int node_index) const {
	return (nodes_[node_index].links.size() == 0);
}

const std::string& SuffixTree::sample() const {
	return sample_;
}

char SuffixTree::non_existing_char() const {
	return non_existing_char_;
}

size_t SuffixTree::Size() const {
	return nodes_.size();
}

int SuffixTree::CreateNode() {
	int node_index = nodes_.size();
	nodes_.push_back(Node());
	return node_index;
}

void SuffixTree::InitDummy(int sample_start_index, int sample_end_index) {
	for (int letter_index = sample_start_index; letter_index < sample_end_index; ++letter_index) {
		if (!HasLink(dummy_, sample_[letter_index])) {
			LinkNodes(dummy_, root_, letter_index, letter_index + 1);
		}
	}
}

void SuffixTree::InitLetterSet(int sample_start_index, int sample_end_index) {
	for (int letter_index = sample_start_index; letter_index < sample_end_index; ++letter_index) {
		letter_set_.insert(sample_[letter_index]);
	}
}

void SuffixTree::InitTree() {
	dummy_ = CreateNode();
	root_ = CreateNode();
	active_point_ = NodeReference(root_, 0, 0);
	InitDummy(0, sample_.size());
	nodes_[root_].suffix_link_node_index = dummy_;
}

void SuffixTree::LinkNodes(int source_node_index, int target_node_index, int sample_start_index, int sample_end_index) {
	char letter = sample_[sample_start_index];
	nodes_[source_node_index].links[letter] = Link(target_node_index, sample_start_index, sample_end_index);
}

bool SuffixTree::HasLink(int node_index, char letter) const {
	LinkMapConstIterator link_map_letter_it = nodes_[node_index].links.find(letter);
	return link_map_letter_it != nodes_[node_index].links.end();
}

SuffixTree::Link SuffixTree::GetLink(int node_index, char letter) const {
	Link link = nodes_[node_index].links.at(letter);
	return link;
}

void SuffixTree::CanonicalizeNodeReference(NodeReference* node_reference) const {
	if (node_reference->sample_end_index == node_reference->sample_start_index) {
		// explicit node
		return;
	} else {
		// implicit node
		Link link_from_ancestor = GetLink(node_reference->closest_ancestor, sample_[node_reference->sample_start_index]);
		while (node_reference->sample_end_index - node_reference->sample_start_index >=
			   link_from_ancestor.sample_end_index - link_from_ancestor.sample_start_index) {
			node_reference->closest_ancestor = link_from_ancestor.target_node_index;
			node_reference->sample_start_index += link_from_ancestor.sample_end_index - link_from_ancestor.sample_start_index;
			if (node_reference->sample_start_index < node_reference->sample_end_index) {
				link_from_ancestor = GetLink(node_reference->closest_ancestor, sample_[node_reference->sample_start_index]);
			}
		}
	}
}

SuffixTree::TestAndSplitResult SuffixTree::TestAndSplit(const NodeReference& node_reference) {
	if (node_reference.sample_end_index == node_reference.sample_start_index) {
		// explicit node
		return TestAndSplitResult(HasLink(node_reference.closest_ancestor, sample_[node_reference.sample_end_index]), node_reference.closest_ancestor);
	} else {
		// implicit node
		Link link_from_node = GetLink(node_reference.closest_ancestor, sample_[node_reference.sample_start_index]);
		int link_letter_index = link_from_node.sample_start_index + node_reference.sample_end_index - node_reference.sample_start_index;
		if (sample_[node_reference.sample_end_index] == sample_[link_letter_index]) {
			return TestAndSplitResult(true, node_reference.closest_ancestor);
		}

		int split_node = CreateNode();
		LinkNodes(node_reference.closest_ancestor, split_node, link_from_node.sample_start_index, link_letter_index);
		LinkNodes(split_node, link_from_node.target_node_index, link_letter_index, link_from_node.sample_end_index);
		return TestAndSplitResult(false, split_node);
	}
}

SuffixTree::NodeReference SuffixTree::AddNextLetter(NodeReference active_point) {
	int previous_node = root_;
	TestAndSplitResult test_and_split_result = TestAndSplit(active_point);
	while (!test_and_split_result.reached_endpoint) {
		// create infinite branch
		LinkNodes(test_and_split_result.node_index, CreateNode(), active_point.sample_end_index, kInfinity);

		// create suffix link for previous node
		if (previous_node != root_) {
			nodes_[previous_node].suffix_link_node_index = test_and_split_result.node_index;
		}
		previous_node = test_and_split_result.node_index;

		// go through suffix_link to next vertex
		active_point.closest_ancestor = nodes_[active_point.closest_ancestor].suffix_link_node_index;
		CanonicalizeNodeReference(&active_point);

		test_and_split_result = TestAndSplit(active_point);
	}
	if (previous_node != root_) {
		nodes_[previous_node].suffix_link_node_index = test_and_split_result.node_index;
	}
	return active_point;
}
