#pragma once
#include <string>
#include <vector>
#include <utility>
#include <cstdlib>

typedef std::vector< std::vector<char> > bricks_t;
typedef std::vector< std::vector<size_t> > graph_t;
const size_t FACE_Q = 6;

size_t maximal_matching(const std::string& word, const bricks_t& bricks);
bool enlarge_matching(size_t letter_index, std::vector<ssize_t>& matching, const graph_t& graph, std::vector<bool>& used);
