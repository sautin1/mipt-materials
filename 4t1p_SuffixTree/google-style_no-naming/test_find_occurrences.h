#ifndef TEST_FIND_OCCURRENCES_H
#define TEST_FIND_OCCURRENCES_H

#include <gtest/gtest.h>
#include <algorithm>
#include "find_occurrences.h"

void PrefixFunction(const std::string& string, std::vector<int>& prefix);

class FindAllOccurrencesTest : public ::testing::Test {
protected:
	FindAllOccurrencesTest()
		: string_length(1e6), sample_cycle("bca"), pattern("abc") {
		sample.reserve(string_length);
		for (size_t cycle_index = 0; cycle_index < cycle_quantity(); ++cycle_index) {
			sample += sample_cycle;
		}
	}

	size_t cycle_quantity() {
		return string_length / sample_cycle.size();
	}

	std::vector<int> FindAllOccurrencesKMP() {
		std::string string_for_kmp(pattern + "$" + sample);
		std::vector<int> prefix_function;
		PrefixFunction(string_for_kmp, prefix_function);
		std::vector<int> occurrences;
		for (size_t char_index = pattern.size() + 1; char_index < string_for_kmp.size(); ++char_index) {
			if (prefix_function[char_index] == (int)pattern.size()) {
				occurrences.push_back(char_index - 2 * pattern.size());
			}
		}
		return occurrences;
	}

	size_t string_length;
	std::string sample_cycle;
	std::string sample;
	std::string pattern;
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

void PrefixFunction(const std::string& string, std::vector<int>& prefix) {
	prefix.resize(string.length(), 0);
	for (int index = 1; index < (int)string.length(); ++index) {
		int match_index = prefix[index-1];
		while (match_index > 0 && string[index] != string[match_index]) {
			match_index = prefix[match_index-1];
		}
		if (string[index] == string[match_index]) {
			++match_index;
		}
		prefix[index] = match_index;
	}
}


TEST_F(FindAllOccurrencesTest, StressTestCyclicString) {
	SuffixTree suffix_tree(sample);
	std::vector<int> occurrences = FindAllOccurrences(suffix_tree, pattern);
	ASSERT_EQ(occurrences.size(), cycle_quantity() - 1);
	for (size_t occurrence_index = 0; occurrence_index < occurrences.size(); ++occurrence_index) {
		ASSERT_EQ(occurrences[occurrence_index] % sample_cycle.size(), 2);
	}
}

TEST_F(FindAllOccurrencesTest, StressTestShuffledString) {
	std::random_shuffle(sample.begin(), sample.end());
	SuffixTree suffix_tree(sample);
	std::vector<int> tested_occurrences = FindAllOccurrences(suffix_tree, pattern);
	std::vector<int> correct_occurrences = FindAllOccurrencesKMP();
	BucketSort(tested_occurrences, sample.size() - 1);
	ASSERT_EQ(tested_occurrences.size(), correct_occurrences.size());
	for (size_t occurrence_index = 0; occurrence_index < correct_occurrences.size(); ++occurrence_index) {
		ASSERT_EQ(tested_occurrences[occurrence_index], correct_occurrences[occurrence_index]);
	}
}

#endif // TEST_FIND_OCCURRENCES_H
