#include "edit_distance.h"

size_t discard_potentials(const cell_t& from, const cell_t& to, const size_t& old_weight)
{
	ssize_t from_potential = from.first - from.second;
	ssize_t to_potential = to.first - to.second;
	ssize_t new_weight = old_weight + std::abs(from_potential) - std::abs(to_potential);
	if (new_weight < 0){
		throw std::logic_error(std::string("Discard potentials: edge weight is negative!"));
	}
	return new_weight;
}

size_t apply_potentials(const cell_t& from, const cell_t& to, const size_t& old_weight)
{
	ssize_t from_potential = from.first - from.second;
	ssize_t to_potential = to.first - to.second;
	ssize_t new_weight = old_weight + std::abs(to_potential) - std::abs(from_potential);
	if (new_weight < 0){
		throw std::logic_error(std::string("Apply potentials: edge weight is negative!"));
	}
	return new_weight;
}

size_t edit_distance(const std::string& source_string1, const std::string& source_string2)
{
	//For statistics
	size_t max_front_size = 0;
	size_t max_front_capacity = 0;
	size_t extract_count = 0;

	std::string s1(source_string1);
	std::string s2(source_string2);
	s1.insert(s1.begin(), 0);
	s2.insert(s2.begin(), 0);

	std::cout << "s1.size = " << s1.size() << "\n";
	std::cout << "s2.size = " << s2.size() << "\n";

	binary_heap_ref<cell_t, size_t, hasher> front;
	cell_t start_cell(0, 0);
	front.insert(std::make_pair(start_cell, 0));
	front_cell_t destination_cell = std::make_pair(cell_t(s1.size()-1, s2.size()-1), 0);
	while (!front.empty()){
		front_cell_t min_front_cell = front.extract_min(s1, s2);
		++extract_count;
		size_t s1_index = min_front_cell.first.first;
		size_t s2_index = min_front_cell.first.second;
		//std::cout << "Extract ( " << s1_index << ", " << s2_index << " ) with " << min_front_cell.second << "\n";
		if (destination_cell.first == min_front_cell.first){
			destination_cell.second = min_front_cell.second;
			break;
		}
		if (s1_index + 1 < s1.size() && s1[s1_index + 1] != s2[s2_index]){
			cell_t cell_to(s1_index + 1, s2_index);
			size_t edge_weight = apply_potentials(min_front_cell.first, cell_to, 1);
			if (front.update_value(std::make_pair(cell_to, min_front_cell.second + edge_weight))){
				//std::cout << "Update/create ( " << cell_to.first << ", " << cell_to.second << " ) with " << min_front_cell.second + edge_weight << "\n";
			}
		}
		if (s2_index + 1 < s2.size() && s1[s1_index] != s2[s2_index + 1]){
			cell_t cell_to(s1_index, s2_index + 1);
			size_t edge_weight = apply_potentials(min_front_cell.first, cell_to, 1);
			if (front.update_value(std::make_pair(cell_to, min_front_cell.second + edge_weight))){
				//std::cout << "Update/create ( " << cell_to.first << ", " << cell_to.second << " ) with " << min_front_cell.second + edge_weight << "\n";
			}
		}
		if (s1_index + 1 < s1.size() && s2_index + 1 < s2.size()){
			cell_t cell_to(s1_index + 1, s2_index + 1);
			if (s1[s1_index + 1] == s2[s2_index + 1]){
				size_t edge_weight = apply_potentials(min_front_cell.first, cell_to, 0);
				if (front.update_value(std::make_pair(cell_to, min_front_cell.second + edge_weight))){
					//std::cout << "Update/create ( " << cell_to.first << ", " << cell_to.second << " ) with " << min_front_cell.second + edge_weight << "\n";
				}
			} else {
				size_t edge_weight = apply_potentials(min_front_cell.first, cell_to, 1);
				if (front.update_value(std::make_pair(cell_to, min_front_cell.second + edge_weight))){
					//std::cout << "Update/create ( " << cell_to.first << ", " << cell_to.second << " ) with " << min_front_cell.second + edge_weight << "\n";
				}
			}
		}
		max_front_size = std::max(front.size(), max_front_size);
		max_front_capacity = std::max(front.capacity(), max_front_capacity);
	}
	//Statistics
	std::cout << "Max heap size: " << max_front_size << "\n";
	std::cout << "Max reference size: " << max_front_capacity << "\n";
	std::cout << "Extract_min's = " << extract_count << "\n";

	destination_cell.second = discard_potentials(start_cell, destination_cell.first, destination_cell.second);
	return destination_cell.second;
}
