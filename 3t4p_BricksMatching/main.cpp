#include <iostream>
#include <fstream>
#include <stdexcept>
#include <string>
#include <vector>
#include "brick_solver.h"

const std::string INPUT_FILE  = "bricks.in";
const std::string OUTPUT_FILE = "bricks.out";

int main()
{
	std::ifstream fin(INPUT_FILE.c_str(), std::ifstream::in);
	if (!fin.good()){
		throw std::runtime_error(std::string("Cannot open file ") + INPUT_FILE);
	}
	size_t brick_quantity;
	fin >> brick_quantity;
	bricks_t bricks(brick_quantity, std::vector<char>());
	for (size_t brick_index = 0; brick_index < brick_quantity; ++brick_index){
		for (size_t face_index = 0; face_index < FACE_Q; ++face_index){
			char tmp;
			fin >> tmp;
			bricks[brick_index].push_back(tmp);
		}
	}
	std::string word;
	fin >> word;
	fin.close();

	bool success = (brick_quantity >= word.size()) && (maximal_matching(word, bricks) == word.size());

	std::ofstream fout(OUTPUT_FILE.c_str(), std::ofstream::out);
	if (!fout.good()){
		throw std::runtime_error(std::string("Cannot open/create output file ") + OUTPUT_FILE);
	}
	if (success){
		fout << "YES\n";
	} else {
		fout << "NO\n";
	}
	fout.close();
}
