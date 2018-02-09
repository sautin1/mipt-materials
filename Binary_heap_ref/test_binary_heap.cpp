#include "test_binary_heap.h"

void tester()
{
    binary_heap_ref<int, std::less<int>, std::hash<int> > heap;
    binary_heap_ref<int, std::less<int>, std::hash<int> > heap2;
    std::vector<int> vect;
    std::vector<bool> used(MAX_KEY, false);
    //insert check
    size_t insert_q = rand() % MAX_INSERT_Q + 2;
    for (size_t element_index = 0; element_index < insert_q/2; ++element_index) {
        vect.push_back(rand() % MAX_KEY);
        while (used[vect.back()]) {
            vect[vect.size()-1] = rand() % MAX_KEY;
        }
        if (!heap.insert(vect.back())) {
            throw std::logic_error("Element was not inserted in insert check!");
        }
        used[vect.back()] = true;
        vect.push_back(rand() % MAX_KEY);
        while (used[vect.back()]) {
            vect[vect.size()-1] = rand() % MAX_KEY;
        }
        heap2.insert(vect.back());
        used[vect.back()] = true;
    }
    if (heap.size() + heap2.size() != vect.size()) {
        throw std::logic_error("Size is wrong after insert!");
    }
    //meld check
    heap.meld(heap2);
    if (heap.size() != vect.size()) {
        throw std::logic_error("Size is wrong after meld!");
    }
    std::sort(vect.begin(), vect.end());
    for (size_t element_index = 0; element_index < vect.size(); ++element_index) {
        if (heap.extractMin() != vect[element_index]) {
            throw std::logic_error("Wrong value after meld!");
        }
    }
    for (size_t element_index = 0; element_index < vect.size(); ++element_index) {
        heap.insert(vect[element_index]);
    }
    //update check
    size_t update_q = rand() % MAX_UPDATE_Q;
    for (size_t element_index = 0; element_index < update_q; ++element_index) {
        size_t index = rand() % vect.size();
        used[vect[index]] = false;
        int newKey = rand() % MAX_KEY;
        while (used[newKey]) {
            newKey = rand() % MAX_KEY;
        }
        if (!heap.updateValue(vect[index], newKey)) {
            throw std::logic_error("Element was not updated in update check!");
        }
        vect[index] = newKey;
        used[newKey] = true;
    }
    if (heap.size() != vect.size()) {
        throw std::logic_error("Size is wrong after update!");
    }
    std::sort(vect.begin(), vect.end());
    for (size_t element_index = 0; element_index < vect.size(); ++element_index) {
        int left = heap.extractMin();
        int right = vect[element_index];
        if (left != right) {
            throw std::logic_error("Wrong value after update!");
        }
    }
    for (size_t element_index = 0; element_index < vect.size(); ++element_index) {
        heap.insert(vect[element_index]);
    }
    //erase_check
    size_t erase_q = rand() % insert_q;
    for (size_t element_index = 0; element_index < erase_q; ++element_index) {
        size_t index = rand() % vect.size();
        while (vect[index] == MAX_KEY) {
            index = rand() % vect.size();
        }
        if (!heap.erase(vect[index])) {
            throw std::logic_error("Element not erased in erase check!");
        }
        vect[index] = MAX_KEY;
    }
    std::vector<int> vect2;
    for (size_t index = 0; index < vect.size(); ++index) {
        if (vect[index] < MAX_KEY) {
            vect2.push_back(vect[index]);
        }
    }
    if (heap.size() != vect2.size()) {
        throw std::logic_error("Size is wrong after erase!");
    }
    for (size_t element_index = 0; element_index < vect2.size(); ++element_index) {
        int left = heap.extractMin();
        int right = vect2[element_index];
        if (left != right) {
            throw std::logic_error("Wrong value after erase!");
        }
    }

    std::cout << "\tInsert: " << insert_q << ". Erase: " << erase_q << ". Update: " << update_q << "\n";
}

void test_binary_heap(size_t test_q)
{
    srand(time(NULL));
    for (size_t test_index = 0; test_index < test_q; ++test_index) {
        std::cout << "Test #" << test_index + 1 << "...\n";
        tester();
        std::cout << "passed!\n\n";
    }
    std::cout << "All tests passed!\n";
}

void test_binary_heap()
{
    test_binary_heap(TEST_Q);
}
