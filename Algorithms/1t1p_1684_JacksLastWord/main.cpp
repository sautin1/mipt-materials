#include <iostream>
#include <vector>
#include <string>

void prefix_function(const std::string& string, std::vector<int>& prefix) {
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

int main()
{
	std::string fence_word;
	std::string jacks_word;
	std::cin >> fence_word >> jacks_word;
	std::vector<int> prefix;
	prefix_function(fence_word + "$" + jacks_word, prefix);
	bool is_unique = false;
	int jacks_word_index = fence_word.length() + 1;
	for (int index = 0; index < (int)jacks_word.length(); ++index) {
		is_unique = (prefix[jacks_word_index + index] == 0);
		if (is_unique) {
			break;
		}
	}

	/* Output to check prefix function values
	std::cout << fence_word + "$" + jacks_word + "\n";
	for (int i = 0; i < (int)prefix.size(); ++i) std::cout << prefix[i];
	std::cout << "\n";
	*/

	if (is_unique) {
		std::cout << "Yes\n";
	} else {
		std::cout << "No\n";
		// print prefixes
		std::vector<int> prefix_sizes;
		for (int index = 0; index < (int)jacks_word.length()-1; ++index) {
			if (prefix[index + jacks_word_index] + 1 > prefix[index + jacks_word_index + 1] ) {
				prefix_sizes.push_back(prefix[index + jacks_word_index] + 1 - prefix[index + jacks_word_index + 1]);
			}
		}
		prefix_sizes.push_back(prefix.back());

		for (int index = 0; index < (int)prefix_sizes.size(); ++index) {
			std::cout << fence_word.substr(0, prefix_sizes[index]) << " ";
		}
	}
	return 0;
}

