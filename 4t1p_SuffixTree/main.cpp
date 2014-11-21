#include <iostream>
#include "suffix_tree.h"

int main()
{
	/*SuffixTree tree("abcdefghijklmnop");
	std::cout << tree.IsSubstring("ghij") << "\n";
	std::cout << tree.IsSubstring("opq") << "\n";
	tree.AppendSample("qrstuvwxyz");
	std::cout << tree.IsSubstring("opq") << "\n";
	std::cout << tree.IsSubstring("vwx") << "\n";*/
	int test_quantity;
	std::cin >> test_quantity;
	for (int test_number = 0; test_number < test_quantity; ++test_number) {
		std::string sample;
		std::cin >> sample;
		SuffixTree tree(sample);
		int query_quantity;
		std::cin >> query_quantity;
		for (int query_number = 0; query_number < query_quantity; ++query_number) {
			std::string substring;
			std::cin >> substring;
			if (tree.IsSubstring(substring)) {
				std::cout << "y\n";
			} else {
				std::cout << "n\n";
			}
		}
	}
}

