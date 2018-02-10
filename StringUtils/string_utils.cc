// Copyright 2014, MIPT
// All rights reserved.
//
// Author: sautin1@yandex.ru (Andrew Sautin)
//
// The StringUtils Framework
//
// This header file defines functions for solving some cool problems on strings.

#include "string_utils.h"

NodeReference::NodeReference(int _closest_ancestor_node, int _sample_start_index,
                             int _sample_end_index)
  : closest_ancestor(_closest_ancestor_node),
    sample_start_index(_sample_start_index),
    sample_end_index(_sample_end_index) {}

SubstringLocation::SubstringLocation(int begin_index, int end_index)
  : begin(begin_index), end(end_index) {}

bool SubstringLocation::Less(const SubstringLocation& other) const {
  return (end - begin) < (other.end - other.begin);
}

bool SubstringLocation::Equals(const SubstringLocation& other) const {
  return (end == other.end && begin == other.begin);
}

std::vector<char> GetFewNonExistingChars(const std::string& sample, size_t char_quantity) {
  std::set<char> letter_set;
  for (size_t sample_index = 0; sample_index < sample.size(); ++sample_index) {
    letter_set.insert(sample[sample_index]);
  }
  std::vector<char> non_existing_chars;
  char max_char = std::numeric_limits<char>::max();
  for (char check_char = '\1'; check_char <= max_char; ++check_char) {
    if (non_existing_chars.size() == char_quantity) {
      break;
    }
    if (letter_set.find(check_char) == letter_set.end()) {
      non_existing_chars.push_back(check_char);
    }
  }
  return non_existing_chars;
}

char GetNonExistingChar(const std::string& sample) {
  return GetFewNonExistingChars(sample, 1).front();
}

size_t DistinctSubstringQuantity(const std::string& sample) {
  SuffixTree suffix_tree(sample, "");
  DistinctSubstringQuantityVisitor visitor;
  suffix_tree.TreeTraversal<DistinctSubstringQuantityVisitor>(&visitor);
  return visitor.substring_quantity();
}

std::vector<int> BuildSuffixArray(const std::string& sample) {
  char non_existing_char = GetNonExistingChar(sample);
  SuffixTree suffix_tree(sample, std::string(1, non_existing_char));
  BuildSuffixArrayVisitor visitor;
  suffix_tree.TreeTraversal<BuildSuffixArrayVisitor>(&visitor);
  return visitor.suffix_array();
}

std::vector<int> LongestCommonSubstringSize(const std::vector<std::string>& sample_vector) {
  int total_sample_size = 0;
  for (size_t sample_index = 0; sample_index < sample_vector.size(); ++sample_index) {
    total_sample_size += sample_vector[sample_index].size();
  }
  std::string joined_samples;
  joined_samples.reserve(total_sample_size + sample_vector.size());
  for (size_t sample_index = 0; sample_index < sample_vector.size(); ++sample_index) {
    joined_samples += sample_vector[sample_index];
  }
  std::vector<char> non_existing_chars = GetFewNonExistingChars(joined_samples,
                                                                sample_vector.size());
  joined_samples = "";
  std::vector<int> sample_number;
  sample_number.reserve(total_sample_size);
  for (size_t sample_index = 0; sample_index < sample_vector.size(); ++sample_index) {
    joined_samples += sample_vector[sample_index];
    joined_samples += non_existing_chars[sample_index];
    sample_number.resize(sample_number.size() + sample_vector[sample_index].size() + 1,
                         sample_index);
  }
  SuffixTree suffix_tree(joined_samples, "");
  LongestCommonSubstringSizeVisitor visitor(sample_vector.size(), sample_number);
  suffix_tree.TreeTraversal<LongestCommonSubstringSizeVisitor>(&visitor);
  std::vector<int> sample_lcs_sizes = visitor.sample_LCS_sizes();
  int max_lcs_size = 0;
  for (int sample_quantity = sample_vector.size() - 2; sample_quantity >= 0; --sample_quantity) {
    if (max_lcs_size > sample_lcs_sizes[sample_quantity]) {
      sample_lcs_sizes[sample_quantity] = max_lcs_size;
    } else {
      max_lcs_size = sample_lcs_sizes[sample_quantity];
    }
  }
  return sample_lcs_sizes;
}
