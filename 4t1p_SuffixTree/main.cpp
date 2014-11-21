#include <iostream>
#include "suffix_tree.h"
#include "substrings.h"

int main()
{
	std::string sample = "banana";
	SuffixTree suffix_tree(sample);
	std::vector<int> occurences;
	occurences = FindAllOccurrences(suffix_tree, "an");
	for (size_t occurence_number = 0; occurence_number < occurences.size(); ++occurence_number) {
		std::cout << occurences[occurence_number] << " ";
	}
	std::cout << "\n";
}

