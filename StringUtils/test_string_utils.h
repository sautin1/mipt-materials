// Copyright 2014, MIPT
// All rights reserved.
//
// Author: sautin1@yandex.ru (Andrew Sautin)
//
// The StringUtils Framework Tester
//
// This header file declares functions for testing API from "string_utils.h".
// Good link for testing suffix array:
// http://www.allisons.org/ll/AlgDS/Strings/Suffix/

#ifndef TEST_STRING_UTILS_H
#define TEST_STRING_UTILS_H

#include "string_utils.h"

#include <gtest/gtest.h>

class TestLongCyclicString : public ::testing::Test {
 protected:
  TestLongCyclicString()
    : sample_length_(1e6), sample_cycle_("ab") {
    sample_.reserve(sample_length_);
    for (size_t cycle_index = 0; cycle_index < CycleQuantity(); ++cycle_index) {
      sample_ += sample_cycle_;
    }
  }

  size_t CycleQuantity() {
    return sample_length_ / sample_cycle_.size();
  }

  size_t sample_length_;
  std::string sample_cycle_;
  std::string sample_;
};

TEST(FewSmallTests, LongestPalindromicSubstring) {
  std::vector<std::string> samples {
    "banana",
    "forgeeksskeegfor",
    "abcbcbb",
    "WOWILIKEEKIL",
    "ThesampletextthatcouldbereadedthesameinbothordersArozaupalanalapuazorA",
    "11",
    "bbaaabb"};//,
    //"ABCGUFCBA"}; // crash test -> algo wrong
  std::vector<SubstringLocation> answers = {SubstringLocation(1, 6),
                                            SubstringLocation(3, 13),
                                            SubstringLocation(1, 6),
                                            SubstringLocation(4, 12),
                                            SubstringLocation(49, 70),
                                            SubstringLocation(0, 2),
                                            SubstringLocation(0, 7)};
  for (size_t sample_index = 0; sample_index < samples.size(); ++sample_index) {
    SubstringLocation tested_substring = LongestPalindromicSubstring(samples[sample_index]);
    ASSERT_TRUE(tested_substring.Equals(answers[sample_index]));
  }
}

TEST(FewSmallTests, DistinctSubstringQuantityTest) {
  std::vector<std::string> samples = {
    "banana",
    "CCCCC",
    "aaba",
    "aaaa",
    "abacaba"
  };
  std::vector<int> answers {15, 5, 8, 4, 21};
  for (size_t sample_index = 0; sample_index < samples.size(); ++sample_index) {
    int tested_quantity = DistinctSubstringQuantity(samples[sample_index]);
    ASSERT_EQ(tested_quantity, answers[sample_index]);
  }
}

TEST(FewSmallTests, BuildSuffixArray) {
  std::vector<std::string> samples = {
    "banana",
    "abracadabra0AbRa4Cad14abra",
    "mississippi",
    "Wolloomooloo",
    "ilovestrings",
    "chocolate"
  };
  std::vector< std::vector<int> > answers {
    std::vector<int>() = {5, 3, 1, 0, 4, 2},
    std::vector<int>() = {11, 20, 16, 21, 12, 17, 14, 25, 10, 15, 22, 7, 0, 3,
                          18, 5, 13, 23, 8, 1, 4, 19, 6, 24, 9, 2},
    std::vector<int>() = {10, 7, 4, 1, 0, 9, 8, 6, 3, 5, 2},
    std::vector<int>() = {0, 2, 9, 3, 6, 11, 1, 8, 5, 10, 7, 4},
    std::vector<int>() = {4, 10, 0, 8, 1, 9, 2, 7, 11, 5, 6, 3},
    std::vector<int>() = {6, 0, 3, 8, 1, 5, 2, 4, 7}
  };
  for (size_t sample_index = 0; sample_index < samples.size(); ++sample_index) {
    std::vector<int> tested_array = BuildSuffixArray(samples[sample_index]);
    ASSERT_EQ(tested_array.size(), answers[sample_index].size());
    for (size_t suffix_index = 0; suffix_index < tested_array.size(); ++suffix_index) {
      ASSERT_EQ(tested_array[suffix_index], answers[sample_index][suffix_index]);
    }
  }
}

TEST(FewSmallTests, LongestCommonSubstringSize) {
  std::vector< std::vector<std::string> > samples {
    std::vector<std::string> {"ABABC", "BABCA", "ABCBA"},
    std::vector<std::string> {"alsdfkjfjkdsal", "fdjskalajfkdsla"},
    std::vector<std::string> {"AAAAAABC", "AAAAAADC", "EEEEEEEFC"},
    std::vector<std::string> {"bars", "bardak", "diffur", "rumba"}
  };
  std::vector< std::vector<int> > answers {
    std::vector<int> {4, 3},
    std::vector<int> {3},
    std::vector<int> {6, 1},
    std::vector<int> {3, 2, 1},
  };
  for (size_t sample_index = 0; sample_index < samples.size(); ++sample_index) {
    std::vector<int> tested_sizes = LongestCommonSubstringSize(samples[sample_index]);
    ASSERT_EQ(tested_sizes.size(), answers[sample_index].size());
    for (size_t suffix_index = 0; suffix_index < tested_sizes.size(); ++suffix_index) {
      ASSERT_EQ(tested_sizes[suffix_index], answers[sample_index][suffix_index]);
    }
  }
}

TEST_F(TestLongCyclicString, LongestPalindromicSubstringTest) {
  SubstringLocation substring = LongestPalindromicSubstring(sample_);
  ASSERT_EQ(substring.end - substring.begin, sample_.size() - 1);
  ASSERT_LE(substring.end, sample_.size());
  ASSERT_GE(substring.begin, 0);
}

TEST_F(TestLongCyclicString, DistinctSubstringQuantityTest) {
  size_t tested_substring_quantity = DistinctSubstringQuantity(sample_);
  size_t correct_substring_quantity = 2 * sample_.size() - 1;
  ASSERT_EQ(tested_substring_quantity, correct_substring_quantity);
}

TEST_F(TestLongCyclicString, BuildSuffixArrayTest) {
  std::vector<int> suffix_array = BuildSuffixArray(sample_);
  int correct_suffix_index = sample_.size() - 2;
  size_t suffix_array_index;
  ASSERT_EQ(suffix_array.size(), sample_.size());
  for (suffix_array_index = 0; suffix_array_index < suffix_array.size(); ++suffix_array_index) {
    ASSERT_EQ(suffix_array[suffix_array_index], correct_suffix_index);
    correct_suffix_index -= 2;
    if (correct_suffix_index < 0) {
      correct_suffix_index = sample_.size() - 1;
    }
  }
}

TEST_F(TestLongCyclicString, LongestCommonSubstringSize) {
  std::vector<std::string> sample_vector{sample_, sample_ + sample_cycle_,
                                         sample_ + sample_cycle_ + sample_cycle_};
  std::vector<int> sample_LCS_sizes = LongestCommonSubstringSize(sample_vector);
  ASSERT_EQ(sample_LCS_sizes.size(), 2);
  ASSERT_EQ(sample_LCS_sizes[0], sample_.size() + 1 * sample_cycle_.size());
  ASSERT_EQ(sample_LCS_sizes[1], sample_.size() + 0 * sample_cycle_.size());
}

#endif // TEST_STRING_UTILS_H
