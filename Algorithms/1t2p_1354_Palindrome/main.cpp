#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

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
    std::string word;
    std::cin >> word;
    std::string reversed_word = word;
    std::reverse(reversed_word.begin(), reversed_word.end());

    std::vector<int> prefix;
    prefix_function(reversed_word + "$" + word, prefix);
    int common_part_size = prefix.back();
    if (common_part_size == (int)word.size()) {
        // initial word already was a palindrome
        common_part_size = prefix[common_part_size - 1];
    }

    word = word.substr(0, word.length() - common_part_size);
    std::cout << word << reversed_word << "\n";
    return 0;
}

