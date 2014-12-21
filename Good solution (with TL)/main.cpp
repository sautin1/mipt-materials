#include <algorithm>
#include <iostream>
#include <string>

#include "hashing.h"

int main() {
	std::string sample;
	std::getline(std::cin, sample);
	StringHash string_hash(sample, DEFAULT_PRIME_BASE, MODULUS_NEAR_LONG_LONG_MAX);

	int query_quantity = 5;
	std::scanf("%d\n", &query_quantity);
	//std::cin >> query_quantity;
	for (int query_index = 0; query_index < query_quantity; ++query_index) {
		//std::string query_name;
		//std::cin >> query_name;
		char query_ending;
		int begin_index;
		std::scanf("%*[^? ]%c%d ", &query_ending, &begin_index);
		if (query_ending == ' ') {
			// change
			char new_char;
			scanf("%c", &new_char);
			string_hash.changeChar(begin_index-1, new_char);
		} else if (query_ending == '?') {
			// palindrome
			int end_index;
			scanf("%d", &end_index);
			if (string_hash.isPalindromicSubstring(begin_index-1, end_index-1)) {
				std::printf("Yes\n");
			} else {
				std::printf("No\n");
			}
		}
		/*if (query_name[0] == 'p') {
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
		}*/
	}
	return 0;
}

