#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <algorithm>

#include "suffix_tree.h"

namespace {

class FindAllOccurencesVisitor : public SuffixTreeVisitor {
 public:
  FindAllOccurencesVisitor(const std::string* pattern) {
    pattern_ = pattern;
  }

  void ProcessLink(int vertex, int incidence_vertex,
                   int begin_substring_index, int end_substring_index,
                   bool* do_transition) {
    int edge_length = end_substring_index - begin_substring_index;

    if (end_substring_index == suffix_tree_string_->size()) {
      *do_transition = false;
      
      if (IsThereTransition(vertex, begin_substring_index, 
                                    end_substring_index)) {
        occurences_.push_back(suffix_tree_string_->size() - 
                              (edge_length + (*distance_from_root_)[vertex]));
      }
    }
    else {
      *do_transition = IsThereTransition(vertex, begin_substring_index,
        end_substring_index);
    }
  }

  std::vector<int> GetOccurences() {
    return occurences_;
  }

 private:
  std::vector<int> occurences_;
  const std::string* pattern_;

  bool IsThereTransition(int vertex, int begin_substring_index,
                                     int end_substring_index) {
    if (static_cast<size_t>((*distance_from_root_)[vertex]) >= pattern_->size()) {
      return true;
    }

    int checking_length = 
      std::min<int>(end_substring_index - begin_substring_index, 
                    pattern_->size() - (*distance_from_root_)[vertex]);

    std::string::const_iterator pattern_begin = pattern_->cbegin() + 
                                                (*distance_from_root_)[vertex];
    std::string::const_iterator pattern_end = pattern_begin + checking_length;
    std::string::const_iterator string_begin = suffix_tree_string_->cbegin() + 
                                               begin_substring_index;

    return std::equal(pattern_begin, pattern_end, string_begin);
  }
};

std::vector<int> FindAllOccurrences(const SuffixTree& tree, const std::string& pattern) {
  FindAllOccurencesVisitor visitor(&pattern);
  tree.TreeTraversal(&visitor);

  return visitor.GetOccurences();
}

}
