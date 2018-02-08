#include <string>
#include <cstdlib>
#include <ctime>
#include <cstring>
#include <fstream>
#include <stdexcept>
#include <iostream>
#include <vector>

const size_t TEST_QUANTITY = 10;
const size_t STRING_MAX_SIZE = 10000;
const std::string ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const std::string INPUT_FILE1 = "f1.in";
const std::string INPUT_FILE2 = "f2.in";

std::string GetRandomString()
{
	std::string s;
	size_t s_size = rand() % STRING_MAX_SIZE;
	for (size_t letter_number = 0; letter_number < s_size; letter_number++){
		s.push_back(ALPHABET[rand() % ALPHABET.size()]);
	}
	return s;
}

char ParseCommand(std::string& command, size_t& position, char& edited_char, char& result_char)
{
	if (command[0] >= '0' && command[0] <= '9'){
		return '0';
	}

	command.erase(0, command.find(" ") + 1);
	position = atoi(command.substr(0, command.find(":")).c_str());

	command.erase(0, command.find(" ") + 1);
	char command_type = command[0];
	
	command.erase(0, command.find("\"") + 1);
	edited_char = command[0];

	if (command_type == 'C'){
		command.erase(0, 2);
		command.erase(0, command.find("\"") + 1);
		result_char = command[0];
	}

	return command_type;
}

size_t WagnerFischer(const std::string& s1, const std::string& s2)
{
	std::vector<size_t> edit_distance;
	for (size_t i = 0; i <= s2.size(); i++){
		edit_distance.push_back(i);
	}
	size_t prev;
	size_t cur;
	for (size_t i = 0; i < s1.size(); i++){
		prev = edit_distance[0];
		edit_distance[0] += 1;
		for (size_t j = 0; j < s2.size(); j++){
			cur = edit_distance[j+1];

			if (s1[i] == s2[j]){
				edit_distance[j+1] = prev;
			}
			else{
				edit_distance[j+1] = 1 + std::min(edit_distance[j+1], std::min(prev, edit_distance[j]));
			}
			prev = cur;
		}
	}

	return edit_distance.back();
}

int main()
{
	srand(time(NULL));
	for (size_t test_number = 0; test_number < TEST_QUANTITY; test_number++){
		std::string test_s1 = GetRandomString();
		std::string test_s2 = GetRandomString();
		//std::cout << test_s1 << " " << test_s2 << "\n";
		size_t test_distance = WagnerFischer(test_s1, test_s2);

		std::ofstream input_file1;
		std::ofstream input_file2;
		input_file1.open(INPUT_FILE1.c_str());
		input_file2.open(INPUT_FILE2.c_str());
		if (!input_file1.is_open() || !input_file2.is_open()){
			throw std::logic_error("TEST: Input files cannot be created");
		}
		input_file1 << test_s1;
		input_file2 << test_s2;
		input_file1.close();
		input_file2.close();

		std::string system_command = "./hirschberg " + INPUT_FILE1 + " " + INPUT_FILE2;
		std::system(system_command.c_str());

		std::ifstream fin;
		fin.open("hirschberg_result.res");
		if (!fin.is_open()){
			throw std::logic_error("TEST: Result file \"hirschberg_result.res\" cannot be opened");
		}
		if (!fin.good()){
			throw std::logic_error("TEST: Output file error");
		}
		
		size_t number_of_commands = 0;
		size_t edit_distance = 0;
		std::vector<size_t> add_positions;
		std::vector<char> add_chars;
		while (true){
			std::string current_command;
			std::getline(fin, current_command);
			size_t edited_position;
			char edited_char;
			char result_char;
			char command_type = ParseCommand(current_command, edited_position, edited_char, result_char);
			if (command_type != '0'){
				number_of_commands ++;
			}
			switch (command_type)
			{
				case 'C':
					if (test_s1[edited_position] != edited_char){
						throw std::logic_error(std::string("TEST: No character \"") + edited_char + std::string("\" at position ") + current_command);
					}
					test_s1[edited_position] = result_char;
					break;
				case 'A':
					add_chars.push_back(edited_char);
					add_positions.push_back(edited_position);
					break;
				case 'D':
					test_s1[edited_position] = 0;
					break;
				case '0':
					edit_distance = atoi(current_command.c_str());
					if (number_of_commands != edit_distance){
						throw std::logic_error("TEST: Wrong number of commands");
					}
					break;
				default:
					throw std::logic_error("TEST: Wrong output format");
			}
			if (command_type == '0'){
				break;
			}
		}
		fin.close();
		for (ssize_t add_number = add_positions.size()-1; add_number >= 0; add_number--){
			test_s1.insert(add_positions[add_number], 1, add_chars[add_number]);
		}
		
		for (size_t char_index = 0; char_index < test_s1.size();) {
		    if (test_s1[char_index] == 0){
		    	test_s1.erase(char_index, 1);
		    }
		    else {
		    	char_index++;
		    }
		}
		if (test_s1 != test_s2){
			throw std::logic_error("TEST: Wrong set of commands");
		}

		//Check edit-distance
		if (test_distance != edit_distance){
			throw std::logic_error("TEST: Wrong edit distance");
		}
		std::cout << "Test #" << test_number + 1 << " passed!\n";
	}
	std::cout << "All tests passed!\n";
	return 0;
}