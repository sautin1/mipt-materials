#include <iostream>
#include <fstream>
#include <stdexcept>
#include <string>
#include "edit_distance.h"

const std::string INPUT_FILE1  = "source1.in";
const std::string INPUT_FILE2  = "source2.in";
const std::string OUTPUT_FILE = "result.out";

std::string get_string(const std::string& filename)
{
	std::string result_string;
	std::ifstream fin(filename.c_str(), std::ifstream::in);
	if (fin.good()){
		std::getline(fin, result_string, '\0');
		fin.close();
	}
	else{
		throw std::runtime_error(std::string("Cannot open file ") + filename);
	}
	return result_string;
}

int main()
{   
	std::string string1 = get_string(INPUT_FILE1);
	std::string string2 = get_string(INPUT_FILE2);;

	size_t result = edit_distance(string1, string2);
	std::cout << result << "\n";

	std::ofstream fout(OUTPUT_FILE.c_str(), std::ofstream::out);
	if (!fout.good()){
		throw std::runtime_error(std::string("Cannot open/create output file ") + OUTPUT_FILE);
	}
	fout << result;
	fout.close();
}
