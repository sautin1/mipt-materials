#include "brick_solver.h"

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
