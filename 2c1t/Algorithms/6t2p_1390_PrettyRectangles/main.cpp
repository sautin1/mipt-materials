#include <iostream>
#include <string>
#include <vector>

#include "hashing.h"

int main()
{
	int height, width;
	std::cin >> height >> width;
	std::vector<std::string> string_vector(height, "");
	for (int vertical_index = 0; vertical_index < height; ++vertical_index) {
		std::cin >> string_vector[vertical_index];
	}
	ImmutableCharTableHash tableHash(string_vector, DEFAULT_PRIME_VERTICAL_BASE,
									 DEFAULT_PRIME_HORIZONTAL_BASE, MODULUS_NEAR_LONG_LONG_MAX);
	int counter = 0;
	for (int end_vertical_index = 0; end_vertical_index < height; ++end_vertical_index) {
		for (int end_horizontal_index = 0; end_horizontal_index < width; ++end_horizontal_index) {
			for (int start_vertical_index = 0; start_vertical_index <= end_vertical_index; ++start_vertical_index) {
				for (int start_horizontal_index = 0; start_horizontal_index <= end_horizontal_index; ++start_horizontal_index) {
					if (tableHash.isPalindromicSubtable(start_vertical_index, start_horizontal_index,
														end_vertical_index, end_horizontal_index)) {
						++counter;
					}
				}
			}
		}
	}
	std::cout << counter << "\n";
	return 0;
}

