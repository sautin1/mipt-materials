#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include <stdexcept>

const std::string INPUT_FILE = "input.txt";
const std::string OUTPUT_FILE = "output.txt";
const size_t INACTIVE = 0;
const size_t Y_EQUAL = 1;
const size_t N_EQUAL_LEFT = 2;
const size_t N_EQUAL_RIGHT = 3;


int main()
{
	std::string source_string;
	std::ifstream fin;
	fin.open(INPUT_FILE.c_str());
	if (fin.good()) {
		std::getline(fin, source_string, '\0');
		fin.close();
	}
	else {
		throw std::runtime_error(std::string("Cannot open file ") + INPUT_FILE);
	}

	std::vector< std::vector<size_t> > palindrome_length;
	std::vector< std::vector<size_t> > reference;

	for (size_t index = 0; index <= source_string.size(); ++index){
		std::vector<size_t> subpalindrome_length;
		subpalindrome_length.push_back(0); // = INACTIVE
		subpalindrome_length.push_back(1);

		palindrome_length.push_back(subpalindrome_length);
		subpalindrome_length[1] = 0; // = INACTIVE 
		reference.push_back(subpalindrome_length);
	}

	for (size_t substr_step = 2; substr_step <= source_string.size(); ++substr_step){
		for (size_t substr_begin = 0; substr_begin <= source_string.size() - substr_step; substr_begin++){
			size_t substr_end = substr_begin + substr_step - 1;
			if (source_string[substr_begin] == source_string[substr_end]){
				palindrome_length[substr_begin].push_back(palindrome_length[substr_begin + 1][substr_step - 2] + 2);
				reference[substr_begin].push_back(Y_EQUAL);
			} else {
				if (palindrome_length[substr_begin][substr_step - 1] >= 
					palindrome_length[substr_begin + 1][substr_step - 1]){
					palindrome_length[substr_begin].push_back(palindrome_length[substr_begin][substr_step - 1]);
					reference[substr_begin].push_back(N_EQUAL_RIGHT);
				} else {
					palindrome_length[substr_begin].push_back(palindrome_length[substr_begin + 1][substr_step - 1]);
					reference[substr_begin].push_back(N_EQUAL_LEFT);
				}
			}
		}
	}
	size_t result = palindrome_length[0][source_string.size()];
	//Backtrace
	size_t substr_step = source_string.size();
	size_t substr_begin = 0;

	while (reference[substr_begin][substr_step] != INACTIVE) {
		switch (reference[substr_begin][substr_step]){
			case N_EQUAL_LEFT:
				source_string[substr_begin] = 0;
				--substr_step;
				++substr_begin;
				break;
			case N_EQUAL_RIGHT:
				source_string[substr_begin + substr_step - 1] = 0;
				--substr_step;
				break;
			case Y_EQUAL:
				substr_step -= 2;
				++substr_begin;
				break;
		}
	}

	for (size_t char_index = 0; char_index < source_string.size();) {
		if (source_string[char_index] == 0){
			source_string.erase(char_index, 1);
		}
		else {
			char_index++;
		}
	}

	std::ofstream fout;
	fout.open(OUTPUT_FILE.c_str());
	if (fout.good()) {
		fout << result << "\n";
		fout << source_string;
		fout.close();
	}
	else {
		throw std::runtime_error(std::string("Cannot create file ") + OUTPUT_FILE);
	}
}