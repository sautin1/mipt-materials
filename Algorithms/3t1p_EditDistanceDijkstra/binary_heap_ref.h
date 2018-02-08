#pragma once
#include <unordered_map>
#include <vector>
#include <iostream>
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

