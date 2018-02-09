#pragma once
#include <unordered_map>
#include <vector>
#include <iostream>
#include <utility>

/* Stores only unique keys
*/
template<typename keyT, typename comparatorT, typename hashT>
class binary_heap_ref
{
    typedef typename std::unordered_map<keyT, size_t, hashT>::iterator reference_it;
    std::vector<keyT> heap;
    std::unordered_map<keyT, size_t, hashT> reference;
    comparatorT isless;

    void heapify_up(size_t heap_index)
    {
        reference_it current_node_it = reference.find(heap[heap_index]);
        size_t parent_index = (heap_index - 1) / 2;
        while (heap_index > 0 && isless(heap[heap_index], heap[parent_index])){
            reference_it parent_it = reference.find(heap[parent_index]);
            std::swap(heap[heap_index], heap[parent_index]);
            std::swap(current_node_it->second, parent_it->second);
            heap_index = parent_index;
            parent_index = (heap_index - 1) / 2;
        }
    }

    void heapify_down(size_t heap_index)
    {
        reference_it current_node_it = reference.find(heap[heap_index]);
        while (heap_index < heap.size()) {
            size_t left_son_index = heap_index * 2 + 1;
            size_t right_son_index = heap_index * 2 + 2;
            if (right_son_index < heap.size()) {
                //both sons exist
                if (isless(heap[left_son_index], heap[right_son_index]) && isless(heap[left_son_index], heap[heap_index])) {
                    //swap with left son
                    reference_it left_son_it = reference.find(heap[left_son_index]);
                    std::swap(heap[heap_index], heap[left_son_index]);
                    std::swap(current_node_it->second, left_son_it->second);
                    heap_index = left_son_index;
                } else if (isless(heap[right_son_index], heap[left_son_index]) && isless(heap[right_son_index], heap[heap_index])) {
                    //swap with right son
                    reference_it right_son_it = reference.find(heap[right_son_index]);
                    std::swap(heap[heap_index], heap[right_son_index]);
                    std::swap(current_node_it->second, right_son_it->second);
                    heap_index = right_son_index;
                } else {
                    break;
                }
            }
            else {
                if (left_son_index < heap.size() && isless(heap[left_son_index], heap[heap_index])) {
                    //only left son exists; swap with left son
                    reference_it left_son_it = reference.find(heap[left_son_index]);
                    std::swap(heap[heap_index], heap[left_son_index]);
                    std::swap(current_node_it->second, left_son_it->second);
                    heap_index = left_son_index;
                }
                break;
            }
        }
    }


public:
    binary_heap_ref(): heap(), reference(){}

    size_t size() const
    {
        return heap.size();
    }

    bool empty() const
    {
        return heap.empty();
    }

    void clear()
    {
        heap.clear();
        reference.clear();
    }

    bool insert(const keyT& newKey)
    {
        bool is_inserted = reference.insert(std::make_pair(newKey, heap.size())).second;
        if (is_inserted) {
            heap.push_back(newKey);
            heapify_up(heap.size()-1);
        }
        return is_inserted;
    }

    keyT extractMin()
    {
        keyT min_element = heap.front();

        reference_it front_it = reference.find(heap.front());
        reference_it back_it = reference.find(heap.back());
        std::swap(heap.front(), heap.back());
        std::swap(front_it->second, back_it->second);
        heap.pop_back();
        reference.erase(front_it);
        heapify_down(0);
        return min_element;
    }

    bool updateValue(const keyT& updateKey, const keyT& newKey)
    {
        reference_it element_it = reference.find(updateKey);
        if (element_it == reference.end()) {
            // this key doesn't exist in heap
            return false;
        }
        size_t keyHeapIndex = element_it->second;
        reference.erase(element_it);
        element_it = reference.insert(std::make_pair(newKey, keyHeapIndex)).first;
        heap[keyHeapIndex] = newKey;
        if (isless(newKey, updateKey)) {
            // decreaseKey
            heapify_up(keyHeapIndex);
        } else if (isless(updateKey, newKey)) {
            // increaseKey
            heapify_down(keyHeapIndex);
        }
        return true;
    }

    bool erase(const keyT& deleteKey)
    {
        reference_it element_it = reference.find(deleteKey);
        if (element_it == reference.end()) {
            // this key doesn't exist in heap
            return false;
        }
        size_t keyHeapIndex = element_it->second;
        reference.erase(element_it);
        if (keyHeapIndex == heap.size()-1) {
            heap.pop_back();
        } else {
            heap[keyHeapIndex] = heap.back();
            reference_it last_element_it = reference.find(heap.back());
            last_element_it->second = keyHeapIndex;
            heap.pop_back();
            if (isless(heap[keyHeapIndex], deleteKey)) {
                heapify_up(keyHeapIndex);
            } else {
                heapify_down(keyHeapIndex);
            }
        }
        return true;
    }

    void meld(binary_heap_ref<keyT, comparatorT, hashT>& rightHeap)
    {
        while (!rightHeap.empty()) {
            insert(rightHeap.extractMin());
        }
    }
};

