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

// Struct NodeReference represents a place where node (either explicit or implicit)
// can be met in the suffix tree.
// This place is represented by:
// 1) closest_ancestor - the index of the closest explicit node on the way
// from root_ node to the place;
// 2) two indices, representing the substring on the label of the link
// from closest_ancestor to the place.
struct NodeReference {
  int closest_ancestor;
  int sample_start_index;
  int sample_end_index;
  NodeReference() = default;
  NodeReference(int _ancestor_node, int _sample_start_index, int _sample_end_index);
};

// Struct SubstringLocation represents a substring of a string.
// begin - index of the starting character of the substring in the initial string.
// end - index of the last character of the substring in the initial string, increased by 1.
// As a result, if sample is an initial string, then the substring is encoded this way:
// sample[begin; end).
struct SubstringLocation {
  int begin;
  int end;
  SubstringLocation(int begin_index, int end_index);
  bool Less(const SubstringLocation& other) const;
  bool Equals(const SubstringLocation& other) const;
};

#define UNUSED_VISITOR_METHOD_ARGUMENT(expr) (void)(expr)
// TraversalVisitor class for SuffixTree.DepthFirstSearchTraversal() method.
// Is designed for the needs of DistinctSubstringQuantity() function.
class DistinctSubstringQuantityVisitor : public SuffixTreeVisitor {
 public:
  DistinctSubstringQuantityVisitor()
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

// TraversalVisitor class for SuffixTree.DepthFirstSearchTraversal() method.
// Is designed for the needs of BuildSuffixArray() function.
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

// TraversalVisitor class for SuffixTree.DepthFirstSearchTraversal() method.
// Is designed for the needs of LongestCommonSubstringSize() function.
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

// Returns a character that cannot be found in sample.
char GetNonExistingChar(const std::string& sample);

// Returns char_quantity unique characters that cannot be found in sample.
std::vector<char> GetFewNonExistingChars(const std::string& sample, size_t char_quantity);

// Counts the number of distinct substrings of the sample string.
size_t DistinctSubstringQuantity(const std::string& sample);

// builds a suffix array on the base of sample string.
std::vector<int> BuildSuffixArray(const std::string& sample);

// For each k in range[2, sample_vector.size()-1] counts the maximal length of a substring,
// which is a common substring of at least k samples in sample_vector.
// The results are stored in a vector with offset of 2,
// e.g. for k == 2 maximal length of a common substring is stored in result_vector[k-2].
// Complexity: O(sample_vector.size() * total_length_of_all_samples)
std::vector<int> LongestCommonSubstringSize(const std::vector<std::string>& sample_vector);

#endif // STRING_UTILS_H
