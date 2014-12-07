// Copyright 2014, MIPT
// All rights reserved.
//
// Author: sautin1@yandex.ru (Andrew Sautin)
//
// The StringUtils Framework Header
//
// This header file declares functions for solving some cool problems on strings.

#ifndef STRING_UTILS_H
#define STRING_UTILS_H

#include <algorithm>
#include <limits>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include "IvanShafranLib/suffix_tree.h"
#include "IvanShafranLib/find_all_occurrences.h"

struct NodeReference {
  int closest_ancestor;
  int sample_start_index;
  int sample_end_index;
  NodeReference() = default;
  NodeReference(int _ancestor_node, int _sample_start_index, int _sample_end_index);
};

struct SubstringLocation {
  int begin;
  int end;
  SubstringLocation(int begin_index, int end_index);
  bool Less(const SubstringLocation& other) const;
  bool Equals(const SubstringLocation& other) const;
};

#define UNUSED_VISITOR_METHOD_ARGUMENT(expr) (void)(expr)
class LongestPalindromicSubstringVisitor : public SuffixTreeVisitor {
public:
  explicit LongestPalindromicSubstringVisitor(const std::string& pattern)
    : pattern_(pattern), longest_common_substring_(0, 0),
      node_reference_(1, 0, 0), use_suffix_link_(false), pattern_index_(0) {}

  void BeforeVertexProcessing(int node) {
    UNUSED_VISITOR_METHOD_ARGUMENT(node);
    use_suffix_link_ = false;
    exists_edge_by_letter_ = false;
  }

  void ProcessLink(int source_node, int target_node,
                   int edge_start_index, int edge_end_index,
                   bool* do_transition) {
    if (pattern_index_ == pattern_.size()) {
      // job finished
      *do_transition = false;
      return;
    }
    if (pattern_[pattern_index_] != suffix_tree_string_->at(edge_start_index)) {
      // wrong edge
      *do_transition = false;
      return;
    } else {
      // edge by needed letter
      exists_edge_by_letter_ = true;
    }
    int edge_size = edge_end_index - edge_start_index;
    if (node_reference_.sample_end_index > node_reference_.sample_start_index) {
      // came here using suffix link
      if (node_reference_.sample_end_index - node_reference_.sample_start_index >= edge_size) {
        // can skip the edge
        pattern_index_ += edge_size;
        node_reference_.sample_start_index += edge_size;
        node_reference_.closest_ancestor = target_node;
        *do_transition = true;
      } else {
        // cannot skip the edge
        use_suffix_link_ = true;
        *do_transition = false;
      }
      return;
    }
    int edge_index;
    int letters_passed = 0;
    for (edge_index = edge_start_index; edge_index < edge_end_index; ++edge_index) {
      if (pattern_index_ == pattern_.size()) {
        break;
      }
      if (pattern_[pattern_index_] != suffix_tree_string_->at(edge_index)) {
        break;
      } else {
        ++letters_passed;
        ++node_reference_.sample_end_index;
        ++pattern_index_;
      }
    }
    int pattern_substring_size = distance_from_root_->at(source_node) + letters_passed;
    int pattern_substring_start = pattern_index_ - pattern_substring_size;
    UpdateLongestCommonSubstring(SubstringLocation(pattern_substring_start, pattern_index_));
    if (edge_index == edge_end_index) {
      // ran through the whole edge
      node_reference_.closest_ancestor = target_node;
      node_reference_.sample_start_index = node_reference_.sample_end_index;
      *do_transition = true;
    } else {
      // the edge is not finished
      use_suffix_link_ = pattern_index_ < pattern_.size();
      if (use_suffix_link_) {
        pattern_index_ -= letters_passed;
      }
      *do_transition = false;
    }
  }

  void ProcessSuffixLink(int source_node, int target_node, bool* do_transition) {
    UNUSED_VISITOR_METHOD_ARGUMENT(target_node);
    *do_transition = (use_suffix_link_ || !exists_edge_by_letter_) &&
                      source_node != 0;
  }

  SubstringLocation longest_common_substring() const {
    return longest_common_substring_;
  }

private:
  void UpdateLongestCommonSubstring(SubstringLocation new_substring) {
    if (longest_common_substring_.Less(new_substring)) {
      longest_common_substring_ = new_substring;
    }
  }

  const std::string& pattern_;
  SubstringLocation longest_common_substring_;
  NodeReference node_reference_;
  bool use_suffix_link_;
  bool exists_edge_by_letter_;
  size_t pattern_index_;
};

class AllSubstringsVisitor : public SuffixTreeVisitor {
 public:
  AllSubstringsVisitor()
    : substring_quantity_(0) {}

  void ProcessLink(int source_node, int target_node,
                   int edge_start_index, int edge_end_index,
                   bool* do_transition) {
    UNUSED_VISITOR_METHOD_ARGUMENT(source_node);
    UNUSED_VISITOR_METHOD_ARGUMENT(target_node);
    substring_quantity_ += edge_end_index - edge_start_index;
    *do_transition = true;
  }

  void ProcessSuffixLink(int source_node, int target_node, bool* do_transition) {
    UNUSED_VISITOR_METHOD_ARGUMENT(source_node);
    UNUSED_VISITOR_METHOD_ARGUMENT(target_node);
    *do_transition = false;
  }

  size_t substring_quantity() const {
    return substring_quantity_;
  }
 private:
  size_t substring_quantity_;
};

class BuildSuffixArrayVisitor : public SuffixTreeVisitor {
 public:
  void ProcessLink(int source_node, int target_node,
                   int edge_start_index, int edge_end_index,
                   bool* do_transition) {
    UNUSED_VISITOR_METHOD_ARGUMENT(source_node);
    UNUSED_VISITOR_METHOD_ARGUMENT(edge_start_index);
    if (edge_end_index == static_cast<int>(suffix_tree_string_->size())) {
      int suffix_size = distance_from_root_->at(target_node);
      if (suffix_size > 1) {
        // should not add $ to suffix array
        suffix_array_.push_back(suffix_tree_string_->size() - suffix_size);
      }
      *do_transition = false;
    } else {
      *do_transition = true;
    }
  }

  void ProcessSuffixLink(int source_node, int target_node, bool* do_transition) {
    UNUSED_VISITOR_METHOD_ARGUMENT(source_node);
    UNUSED_VISITOR_METHOD_ARGUMENT(target_node);
    *do_transition = false;
  }

  const std::vector<int>& suffix_array() const {
    return suffix_array_;
  }
 private:
  std::vector<int> suffix_array_;
};

class LongestCommonSubstringSizeVisitor : public SuffixTreeVisitor {
 public:
  LongestCommonSubstringSizeVisitor(int sample_quantity, const std::vector<int>& sample_number)
    : sample_quantity_(sample_quantity), sample_number_(sample_number),
      sample_LCS_sizes_(sample_quantity - 1, 0) {}

  void ProcessLink(int source_node, int target_node,
                   int edge_start_index, int edge_end_index,
                   bool* do_transition) {
    UNUSED_VISITOR_METHOD_ARGUMENT(source_node);
    UNUSED_VISITOR_METHOD_ARGUMENT(target_node);
    UNUSED_VISITOR_METHOD_ARGUMENT(edge_start_index);
    if (edge_end_index == static_cast<int>(suffix_tree_string_->size())) {
      // target_node is a leaf
      int sample_index = sample_number_[edge_start_index];
      node_samples_reached_[source_node].insert(sample_index);
      *do_transition = false;
    } else {
      *do_transition = true;
    }
  }

  void ProcessSuffixLink(int source_node, int target_node, bool* do_transition) {
    UNUSED_VISITOR_METHOD_ARGUMENT(source_node);
    UNUSED_VISITOR_METHOD_ARGUMENT(target_node);
    *do_transition = false;
  }

  void AfterVertexProcessing(int node) {
    int reached_samples_quantity = node_samples_reached_[node].size();
    if (reached_samples_quantity > 1) {
      int max_LCS_size = std::max(sample_LCS_sizes_[reached_samples_quantity - 2],
          distance_from_root_->at(node));
      sample_LCS_sizes_[reached_samples_quantity - 2] = max_LCS_size;
    }

    int parent_node = parent_->at(node);
    ReachedSamplesSetConstIterator it = node_samples_reached_[node].begin();
    for (; it != node_samples_reached_[node].end(); ++it) {
      node_samples_reached_[parent_node].insert(*it);
    }
  }

  const std::vector<int>& sample_LCS_sizes() const {
    return sample_LCS_sizes_;
  }
 private:
  typedef std::unordered_set<int> ReachedSamplesSet;
  typedef std::unordered_set<int>::const_iterator ReachedSamplesSetConstIterator;

  int sample_quantity_;
  const std::vector<int>& sample_number_;
  // external unordered_map maps from node index to a set of all samples, which
  // contain the string represented by node.
  std::unordered_map<int, ReachedSamplesSet> node_samples_reached_;
  std::vector<int> sample_LCS_sizes_;
};

#undef UNUSED_VISITOR_METHOD_ARGUMENT

char GetNonExistingChar(const std::string& sample);
std::vector<char> GetFewNonExistingChars(const std::string& sample, size_t char_quantity);

SubstringLocation LongestPalindromicSubstring(const std::string& sample);
size_t DistinctSubstringQuantity(const std::string& sample);
std::vector<int> BuildSuffixArray(const std::string& sample);
std::vector<int> LongestCommonSubstringSize(const std::vector<std::string>& sample_vector);

#endif // STRING_UTILS_H
