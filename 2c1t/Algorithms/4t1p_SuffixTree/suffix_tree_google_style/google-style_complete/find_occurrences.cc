// Copyright 2014, MIPT
// All rights reserved.
//
// Author: sautin1@yandex.ru (Andrew Sautin)
//
// The Suffix Tree C++ Library Tester
//
// This file defines the public API for searching substring occurrences in a string.
//
// Function FindAllOccurrences can be used to find all occurrences of a substring in a string.

#include "find_occurrences.h"

#include <algorithm>
#include <string>

#define UNUSED_VISITOR_METHOD_ARGUMENT(expr) (void)(expr)
FindOccurrencesTraversalVisitor::FindOccurrencesTraversalVisitor(const SuffixTree& suffix_tree,
                                                                 const std::string& pattern)
  : suffix_tree_(suffix_tree), pattern_(pattern) {}

void FindOccurrencesTraversalVisitor::InitVisitor() {
  occurrences_.clear();
  pattern_index_ = 0;
  suffix_length_ = 0;
  pattern_end_node_ = 0;
}

void FindOccurrencesTraversalVisitor::DiscoverNode(const SuffixTree::Link& in_link) {
  int active_node = in_link.target_node_index;
  if (pattern_end_node_ == 0 && pattern_index_ == static_cast<int>(pattern_.size())) {
    pattern_end_node_ = active_node;
  }
  if (pattern_end_node_ > 0 && suffix_tree_.IsLeaf(active_node)) {
    occurrences_.push_back(suffix_tree_.sample().size() - suffix_length_ - pattern_.size());
  }
}

void FindOccurrencesTraversalVisitor::ReturnToNode(const SuffixTree::Link& return_link,
                                                   const SuffixTree::Link& in_link) {
  UNUSED_VISITOR_METHOD_ARGUMENT(in_link);
  if (pattern_end_node_ > 0) {
    if (suffix_tree_.IsLeaf(return_link.target_node_index)) {
      suffix_length_ -= suffix_tree_.sample().size() - return_link.sample_start_index;
    } else {
      suffix_length_ -= return_link.sample_end_index - return_link.sample_start_index;
    }
  }
}

void FindOccurrencesTraversalVisitor::ExamineEdge(const SuffixTree::Link& link) {
  if (pattern_end_node_ > 0) {
    if (suffix_tree_.IsLeaf(link.target_node_index)) {
      suffix_length_ += suffix_tree_.sample().size() - link.sample_start_index;
    } else {
      suffix_length_ += link.sample_end_index - link.sample_start_index;
    }
  } else if (pattern_end_node_ == 0) {
    std::string sample = suffix_tree_.sample();
    int sample_start_index = link.sample_start_index;
    int sample_end_index = std::min(link.sample_end_index, static_cast<int>(sample.size()));
    int sample_letter_index = sample_start_index + 1;
    for (; sample_letter_index < sample_end_index; ++sample_letter_index) {
      if (pattern_index_ == static_cast<int>(pattern_.size())) {
        suffix_length_ += sample_end_index - sample_letter_index;
        break;
      }
      if (sample[sample_letter_index] != pattern_[pattern_index_]) {
        pattern_end_node_ = -1;
        break;
      }
      ++pattern_index_;
    }
  }
}

SuffixTree::DFSChooseNextNeighbourResult FindOccurrencesTraversalVisitor::ChooseNextNeighbour(
    int active_node,
    const LinkMapConstIterator& link_map_begin_it,
    const LinkMapConstIterator& link_map_next_letter_it,
    const LinkMapConstIterator& link_map_end_it,
    int suffix_link) {
  UNUSED_VISITOR_METHOD_ARGUMENT(link_map_begin_it);
  UNUSED_VISITOR_METHOD_ARGUMENT(suffix_link);
  if (pattern_end_node_ == -1) {
    return SuffixTree::DFSChooseNextNeighbourResult(false, link_map_end_it);
  } else if (pattern_end_node_ == 0) {
    LinkMapConstIterator link_map_letter_it;
    link_map_letter_it = suffix_tree_.GetLinkIterator(active_node, pattern_[pattern_index_++]);
    if (link_map_letter_it == link_map_end_it) {
      pattern_end_node_ = -1;
    }
    return SuffixTree::DFSChooseNextNeighbourResult(false, link_map_letter_it);
  } else {
    return SuffixTree::DFSChooseNextNeighbourResult(false, link_map_next_letter_it);
  }
}

void FindOccurrencesTraversalVisitor::FinishNode(const SuffixTree::Link& in_link) {
  if (in_link.target_node_index == pattern_end_node_) {
    pattern_end_node_ = -1;
  }
}

const std::vector<int>& FindOccurrencesTraversalVisitor::occurrences() const {
  return occurrences_;
}

std::vector<int> FindAllOccurrences(const SuffixTree& suffix_tree,
                                    const std::string& search_string) {
  FindOccurrencesTraversalVisitor visitor(suffix_tree, search_string);
  suffix_tree.DepthFirstSearchTraversal<FindOccurrencesTraversalVisitor>(&visitor);
  return visitor.occurrences();
}
#undef UNUSED_VISITOR_METHOD_ARGUMENT
