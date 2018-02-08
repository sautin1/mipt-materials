#include <iostream>
#include <fstream>
#include <stdexcept>
#include <string>

#include <unordered_map>
#include <vector>
#include <utility>

template<typename index_t, typename value_t, typename hash_t>
class binary_heap_ref
{
	struct mapped_t{
		mapped_t(){}
		mapped_t(size_t heap_index_, char updated_, bool extracted_): heap_index(heap_index_), updated(updated_), extracted(extracted_){}
		size_t heap_index;
		char updated;
		bool extracted;
	};

	typedef std::pair<index_t, value_t> heap_element_t;
	typedef typename std::unordered_map<index_t, mapped_t, hash_t>::iterator reference_it;
	typedef typename std::unordered_map<index_t, mapped_t, hash_t>::const_iterator reference_cit;
	std::vector<heap_element_t> heap;
	std::unordered_map<index_t, mapped_t, hash_t> reference;
public:
	binary_heap_ref(): heap(), reference(){}
	size_t size() const
	{
		return heap.size();
	}
	size_t capacity()
	{
		return reference.size();
	}

	bool empty() const
	{
		return heap.empty();
	}

	void heapify_up(size_t heap_index)
	{
		reference_it current_node_it = reference.find(heap[heap_index].first);
		size_t parent_index = (heap_index - 1) / 2;
		while (heap_index > 0 && heap[heap_index].second < heap[parent_index].second){
			reference_it parent_it = reference.find(heap[parent_index].first);
			std::swap(heap[heap_index], heap[parent_index]);
			std::swap(current_node_it->second.heap_index, parent_it->second.heap_index);
			heap_index = parent_index;
			parent_index = (heap_index - 1) / 2;
		}
	}

	void heapify_down(size_t heap_index)
	{
		reference_it current_node_it = reference.find(heap[heap_index].first);
		while (heap_index < heap.size()){
			size_t left_son_index = heap_index * 2 + 1;
			size_t right_son_index = heap_index * 2 + 2;
			if (right_son_index < heap.size()){
				//both sons exist
				if (heap[left_son_index].second <= heap[right_son_index].second && heap[left_son_index].second < heap[heap_index].second){
					//swap with left son
					reference_it left_son_it = reference.find(heap[left_son_index].first);
					std::swap(heap[heap_index], heap[left_son_index]);
					std::swap(current_node_it->second.heap_index, left_son_it->second.heap_index);
					heap_index = left_son_index;
				} else if (heap[right_son_index].second <= heap[left_son_index].second && heap[right_son_index].second < heap[heap_index].second){
					//swap with right son
					reference_it right_son_it = reference.find(heap[right_son_index].first);
					std::swap(heap[heap_index], heap[right_son_index]);
					std::swap(current_node_it->second.heap_index, right_son_it->second.heap_index);
					heap_index = right_son_index;
				} else {
					break;
				}
			}
			else {
				if (left_son_index < heap.size() && heap[left_son_index].second < heap[heap_index].second){
					//only left son exists; swap with left son
					reference_it left_son_it = reference.find(heap[left_son_index].first);
					std::swap(heap[heap_index], heap[left_son_index]);
					std::swap(current_node_it->second.heap_index, left_son_it->second.heap_index);
					heap_index = left_son_index;
				}
				break;
			}
		}
	}

	bool insert(const heap_element_t& new_element)
	{
		bool is_inserted = reference.insert(std::make_pair(new_element.first, mapped_t(heap.size(), 1, false))).second;
		if (is_inserted){
			heap.push_back(new_element);
			heapify_up(heap.size()-1);
		}
		return is_inserted;
	}

	heap_element_t extract_min(const std::string& s1, const std::string& s2)
	{
		heap_element_t min_element = heap.front();

		reference_it front_it = reference.find(heap.front().first);
		reference_it back_it = reference.find(heap.back().first);
		front_it->second.extracted = true;
		std::swap(front_it->second.heap_index, back_it->second.heap_index);
		std::swap(heap.front(), heap.back());
		heap.pop_back();
		size_t s1_index = front_it->first.first;
		size_t s2_index = front_it->first.second;
		if (s1[s1_index] == s2[s2_index] || s1_index == 0 || s2_index == 0){
			//only one edge goes into this cell
			reference.erase(front_it);
		}
		heapify_down(0);
		return min_element;
	}

	bool update_value(const heap_element_t& updated_element)
	{
		bool is_changed = false;
		reference_it element_it = reference.find(updated_element.first);
		if (element_it == reference.end()){
			//this cell is processed for the first time
			is_changed = true;
			insert(updated_element);
		} else {
			++element_it->second.updated;
			if (!element_it->second.extracted){
				//this cell exists in the heap
				if (updated_element.second < heap[element_it->second.heap_index].second){
					is_changed = true;
					heap[element_it->second.heap_index].second = updated_element.second;
					heapify_up(element_it->second.heap_index);
				}
			} else {
				//this cell has already been extracted as minimum
				if (element_it->second.updated == 3){
					reference.erase(element_it);
				}
			}
		}
		return is_changed;
	}
};



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
	/*size_t max_front_size = 0;
	size_t max_front_capacity = 0;
	size_t extract_count = 0;*/

	std::string s1(source_string1);
	std::string s2(source_string2);
	s1.insert(s1.begin(), 0);
	s2.insert(s2.begin(), 0);

	/*std::cout << "s1.size = " << s1.size() << "\n";
	std::cout << "s2.size = " << s2.size() << "\n";*/

	binary_heap_ref<cell_t, size_t, hasher> front;
	cell_t start_cell(0, 0);
	front.insert(std::make_pair(start_cell, 0));
	front_cell_t destination_cell = std::make_pair(cell_t(s1.size()-1, s2.size()-1), 0);
	while (!front.empty()){
		front_cell_t min_front_cell = front.extract_min(s1, s2);
		//++extract_count;
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
		/*max_front_size = std::max(front.size(), max_front_size);
		max_front_capacity = std::max(front.capacity(), max_front_capacity);*/
	}
	//Statistics
	/*std::cout << "Max heap size: " << max_front_size << "\n";
	std::cout << "Max reference size: " << max_front_capacity << "\n";
	std::cout << "Extract_min's = " << extract_count << "\n";*/

	destination_cell.second = discard_potentials(start_cell, destination_cell.first, destination_cell.second);
	return destination_cell.second;
}



const std::string INPUT_FILE  = "input.txt";
const std::string OUTPUT_FILE = "output.txt";

int main()
{
	std::string string1;
	std::string string2;
	std::ifstream fin(INPUT_FILE.c_str(), std::ifstream::in);
	if (fin.good()){
		std::getline(fin, string1);
		std::getline(fin, string2);
		fin.close();
	}
	else{
		throw std::runtime_error(std::string("Cannot open file ") + INPUT_FILE);
	}

	size_t result = edit_distance(string1, string2);
	//std::cout << result << "\n";

	std::ofstream fout(OUTPUT_FILE.c_str(), std::ofstream::out);
	if (!fout.good()){
		throw std::runtime_error(std::string("Cannot open/create output file ") + OUTPUT_FILE);
	}
	fout << result;
	fout.close();
}
