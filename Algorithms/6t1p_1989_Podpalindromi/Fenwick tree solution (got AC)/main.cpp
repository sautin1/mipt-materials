#include <algorithm>
#include <iostream>
#include <string>

#include "hashing.h"

int main() {
	std::ios_base::sync_with_stdio(false);
	std::string sample;
	std::getline(std::cin, sample);
	StringHash string_hash(sample, DEFAULT_PRIME_BASE, MODULUS_NEAR_LONG_LONG_MAX);

	int query_quantity;
	std::cin >> query_quantity;
	for (int query_index = 0; query_index < query_quantity; ++query_index) {
		std::string query_name;
		std::cin >> query_name;
		if (query_name[0] == 'p') {
			int left_index, right_index;
			std::cin >> left_index >> right_index;
			if (string_hash.isPalindromicSubstring(left_index-1, right_index-1)) {
				std::cout << "Yes\n";
			} else {
				std::cout << "No\n";
			}
		} else if (query_name[0] == 'c') {
			int index;
			char new_char;
			std::cin >> index >> new_char;
			string_hash.changeChar(index-1, new_char);
		}
	}
	return 0;
}

