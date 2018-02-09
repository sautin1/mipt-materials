#include "test_system.h"

void binomial_heap_tester()
{
    //statistics
    size_t dk_quantity = 0;
    size_t ik_quantity = 0;
    size_t er_quantity = 0;

    size_t heap_quantity = rand() % MAX_MELD_Q + 1;

    binomial_heap< int, std::less<int> > heap_all;
    std::vector<int> elements;
    for (size_t heap_index = 0; heap_index < heap_quantity; ++heap_index){
        std::vector<binomial_tree_ptr> forest;
        binomial_heap< int, std::less<int> > heap_part;
        size_t heap_size = rand() % (MAX_HEAP_SIZE / MAX_MELD_Q) + 1;
        for (size_t element_index = 0; element_index < heap_size; ++element_index){
            elements.push_back(rand());
            binomial_tree_ptr new_ptr = heap_part.insert(elements.back());
            forest.push_back(new_ptr);
        }
        if (heap_part.size() != heap_size){
            throw std::logic_error("Tester: wrong heap size after insert()");
        }

        //change or erase (one action per heap, otherwise forest can become invalid)
        int action = rand() % 2;
        if (action == 1){
            //change
            size_t changed_element_index = rand() % heap_size;
            int new_value = rand();
            if (new_value < elements[elements.size() - 1 - changed_element_index]){
                ++dk_quantity;
            } else if (new_value > elements[elements.size() - 1 - changed_element_index]){
                ++ik_quantity;
            }
            elements[elements.size() - 1 - changed_element_index] = new_value;
            heap_part.change_key(forest[forest.size() - 1 - changed_element_index], new_value);
        } else {
            //erase last inserted element
            ++er_quantity;
            elements.pop_back();
            heap_part.erase(forest.back());
            forest.pop_back();
            --heap_size;
        }
        if (heap_part.size() != heap_size){
            throw std::logic_error("Tester: wrong heap size after change_key() or erase()");
        }

        heap_all.meld(heap_part);
        if (heap_all.size() != elements.size()){
            throw std::logic_error("Tester: wrong heap size after meld()");
        }
    }

    //sorting
    std::sort(elements.begin(), elements.end());

    for (size_t element_index = 0; element_index < elements.size(); ++element_index){
        int min = heap_all.extract_min(); //exception can be thrown if heap is empty
        if (min != elements[element_index]){
            throw std::logic_error(std::string("Test_system: minimum is wrong"));
        }
    }
    if (heap_all.size() != 0){
        throw std::logic_error("Tester: wrong heap size after extract_min()");
    }

    //output statistics
    std::cout << "Size: " << elements.size() << ". Melds: " << heap_quantity
              << ". DKs: " << dk_quantity << ". IKs: " << ik_quantity << ". ERs: " << er_quantity << "\n";
}

void binomial_heap_test_caller(size_t test_quantity)
{
    srand(time(0));
    for (size_t test_number = 0; test_number < test_quantity; ++test_number){
        binomial_heap_tester();
        std::cout << "Test #" << test_number + 1 << " is passed!\n\n";
    }
    std::cout << "All tests are passed!\n";
}

void binomial_heap_test_caller()
{
    binomial_heap_test_caller(TEST_Q);
}
