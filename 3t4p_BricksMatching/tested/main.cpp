#include <iostream>
#include <fstream>
#include <stdexcept>
#include <string>
#include <vector>

#include <utility>
#include <cstdlib>

typedef std::vector< std::vector<char> > bricks_t;
typedef std::vector< std::vector<size_t> > graph_t;
typedef std::vector<ssize_t> matching_t;
const size_t FACE_Q = 6;

size_t maximal_matching(const std::string& word, const bricks_t& bricks, matching_t& matching);
bool enlarge_matching(size_t letter_index, std::vector<ssize_t>& matching, const graph_t& graph, std::vector<bool>& used);

bool enlarge_matching(size_t letter_index, std::vector<ssize_t>& matching, const graph_t& graph, std::vector<bool>& used)
{
	if (used[letter_index]){
		return false;
	}
	used[letter_index] = true;
	for (size_t neighbour_index = 0; neighbour_index < graph[letter_index].size(); ++neighbour_index){
		size_t to = graph[letter_index][neighbour_index];
		if (matching[to] == -1 || enlarge_matching(matching[to], matching, graph, used)){
			//reverse edge
			matching[to] = letter_index;
			return true;
		}
	}
	return false;
}

size_t maximal_matching(const std::string& word, const bricks_t& bricks, matching_t& matching)
{
	//build graph
	graph_t graph(word.size(), std::vector<size_t>());
	for (size_t letter_index = 0; letter_index < word.size(); ++letter_index){
		for (size_t brick_index = 0; brick_index < bricks.size(); ++brick_index){
			for (size_t face_index = 0; face_index < FACE_Q; ++face_index){
				if (word[letter_index] == bricks[brick_index][face_index]){
					graph[letter_index].push_back(brick_index);
					break;
				}
			}
		}
	}

	size_t max_matching_size = 0;
	matching.assign(bricks.size(), -1);

	//make random matching
	std::vector<bool> matched(word.size(), false);
	for (size_t letter_index = 0; letter_index < word.size(); ++letter_index){
		for (size_t neighbour_index = 0; neighbour_index < graph[letter_index].size(); ++neighbour_index){
			if (matching[graph[letter_index][neighbour_index]] == -1){
				matching[graph[letter_index][neighbour_index]] = letter_index;
				matched[letter_index] = true;
				break;
			}
		}
	}

	//get maximal matching
	for (size_t letter_index = 0; letter_index < word.size(); ++letter_index){
		if (!matched[letter_index]){
			std::vector<bool> used(word.size(), false);
			if (enlarge_matching(letter_index, matching, graph, used)){
				++max_matching_size;
			}
		} else {
			++max_matching_size;
		}
	}
	return max_matching_size;
}


const std::string INPUT_FILE  = "input.txt";
const std::string OUTPUT_FILE = "output.txt";

int main()
{
	std::ifstream fin(INPUT_FILE.c_str(), std::ifstream::in);
	if (!fin.good()){
		throw std::runtime_error(std::string("Cannot open file ") + INPUT_FILE);
	}
	size_t brick_quantity;
	fin >> brick_quantity;
	
	std::string word;
	fin >> word;
	bricks_t bricks(brick_quantity, std::vector<char>());
	for (size_t brick_index = 0; brick_index < brick_quantity; ++brick_index){
		for (size_t face_index = 0; face_index < FACE_Q; ++face_index){
			char tmp;
			fin >> tmp;
			bricks[brick_index].push_back(tmp);
		}
	}
	fin.close();

	matching_t matching;
	bool success = (brick_quantity >= word.size()) && (maximal_matching(word, bricks, matching) == word.size());

	std::ofstream fout(OUTPUT_FILE.c_str(), std::ofstream::out);
	if (!fout.good()){
		throw std::runtime_error(std::string("Cannot open/create output file ") + OUTPUT_FILE);
	}
	if (success){
		fout << "YES\n";
		for (size_t letter_index = 0; letter_index < word.size(); ++letter_index){
			for (size_t brick_index = 0; brick_index < brick_quantity; ++brick_index){
				if (matching[brick_index] == letter_index){
					fout << brick_index + 1 << " ";
				}
			}
		}
	} else {
		fout << "NO\n";
	}
	fout.close();
}
