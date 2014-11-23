#include <iostream>
#include "suffix_tree.h"
#include "find_occurrences.h"

int main()
{
	std::string sample = "abaac";
	SuffixTree suffix_tree(sample);
	std::vector<int> occurences;
	occurences = FindAllOccurrences(suffix_tree, "aab");
	for (size_t occurence_number = 0; occurence_number < occurences.size(); ++occurence_number) {
		std::cout << occurences[occurence_number] << " ";
	}
	std::cout << "\n";

	return 0;
}

