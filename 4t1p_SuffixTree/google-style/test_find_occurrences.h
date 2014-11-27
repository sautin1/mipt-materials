// Copyright 2014, MIPT
// All rights reserved.
//
// Author: sautin1@yandex.ru (Andrew Sautin)
//
// The Suffix Tree C++ Library Tester
//
// This header file declares the public API for testing API from "find_occurrences.h".

#ifndef TEST_FIND_OCCURRENCES_H
#define TEST_FIND_OCCURRENCES_H

#include "find_occurrences.h"

#include <algorithm>

#include <gtest/gtest.h>

void PrefixFunction(const std::string& string, std::vector<int>& prefix_function);

class FindAllOccurrencesTest : public ::testing::Test {
 protected:
  FindAllOccurrencesTest()
    : string_length_(1e6), sample_cycle_("bca"), pattern_("abc"), occurrences_() {
    sample_.reserve(string_length_);
    for (size_t cycle_index = 0; cycle_index < CycleQuantity(); ++cycle_index) {
      sample_ += sample_cycle_;
    }
  }

  size_t CycleQuantity() {
    return string_length_ / sample_cycle_.size();
  }

  void FindAllOccurrencesKnuthMorrisPratt() {
    std::string string_for_kmp(pattern_ + "$" + sample_);
    std::vector<int> prefix_function;
    PrefixFunction(string_for_kmp, prefix_function);
    size_t char_index;
    for (char_index = pattern_.size() + 1; char_index < string_for_kmp.size(); ++char_index) {
      if (prefix_function[char_index] == static_cast<int>(pattern_.size())) {
        occurrences_.push_back(char_index - 2 * pattern_.size());
      }
    }
  }

  size_t string_length_;
  std::string sample_cycle_;
  std::string sample_;
  std::string pattern_;
  std::vector<int> occurrences_;
};

void BucketSort(std::vector<int>& unsorted_vector, size_t max_value) {
  std::vector<int> count(max_value + 1, 0);
  for (size_t element_index = 0; element_index < unsorted_vector.size(); ++element_index) {
    ++count[unsorted_vector[element_index]];
  }
  for (size_t value_index = 1; value_index < count.size(); ++value_index) {
    count[value_index] += count[value_index - 1];
  }
  std::vector<int> sorted_vector(unsorted_vector.size(), 0);
  for (int element_index = unsorted_vector.size() - 1; element_index >= 0; --element_index) {
    sorted_vector[--count[unsorted_vector[element_index]]] = unsorted_vector[element_index];
  }
  unsorted_vector = sorted_vector;
}

void PrefixFunction(const std::string& string, std::vector<int>& prefix_function) {
  prefix_function.resize(string.length(), 0);
  for (int index = 1; index < static_cast<int>(string.length()); ++index) {
    int match_index = prefix_function[index-1];
    while (match_index > 0 && string[index] != string[match_index]) {
      match_index = prefix_function[match_index-1];
    }
    if (string[index] == string[match_index]) {
      ++match_index;
    }
    prefix_function[index] = match_index;
  }
}

TEST_F(FindAllOccurrencesTest, StressTestCyclicString) {
  SuffixTree suffix_tree(sample_);
  std::vector<int> occurrences = FindAllOccurrences(suffix_tree, pattern_);
  ASSERT_EQ(occurrences.size(), CycleQuantity() - 1);
  for (size_t occurrence_index = 0; occurrence_index < occurrences.size(); ++occurrence_index) {
    ASSERT_EQ(occurrences[occurrence_index] % sample_cycle_.size(), 2);
  }
}

TEST_F(FindAllOccurrencesTest, StressTestShuffledString) {
  std::random_shuffle(sample_.begin(), sample_.end());
  SuffixTree suffix_tree(sample_);
  std::vector<int> tested_occurrences = FindAllOccurrences(suffix_tree, pattern_);
  FindAllOccurrencesKnuthMorrisPratt();
  BucketSort(tested_occurrences, sample_.size() - 1);
  ASSERT_EQ(tested_occurrences.size(), occurrences_.size());
  for (size_t occurrence_index = 0; occurrence_index < occurrences_.size(); ++occurrence_index) {
    ASSERT_EQ(tested_occurrences[occurrence_index], occurrences_[occurrence_index]);
  }
}

#endif  // TEST_FIND_OCCURRENCES_H
