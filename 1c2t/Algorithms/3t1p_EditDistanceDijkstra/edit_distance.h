#pragma once
#include "binary_heap_ref.h"
#include <string>
#include <iostream>
#include <stdexcept>

class cell_t{
public:
	cell_t(size_t first_index, size_t second_index): first(first_index), second(second_index){}
	bool operator == (const cell_t& cell) const
	{
		return (cell.first == first && cell.second == second);
	}

	size_t first;
	size_t second;
};

typedef std::pair<cell_t, size_t> front_cell_t;

struct hasher{
	size_t operator()(const cell_t cell) const
	{
		return cell.first + cell.second;
	}
};

size_t discard_potentials(const cell_t& from, const cell_t& to, const size_t& old_weight);
size_t apply_potentials(const cell_t& from, const cell_t& to, const size_t& old_weight);
size_t edit_distance(const std::string& source_string1, const std::string& source_string2);
