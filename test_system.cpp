#include "test_system.h"

void binomial_heap_tester()
{
	//statistics
	size_t dk_quantity = 0;
	size_t ik_quantity = 0;

	size_t heap_quantity = rand() % MAX_MELD_Q + 1;

	binomial_heap heap_all;
	std::vector<int> elements;
	for (size_t heap_index = 0; heap_index < heap_quantity; ++heap_index){
		std::vector< std::shared_ptr<node> > shptrs;
		binomial_heap heap_part;
		size_t heap_size = rand() % (MAX_HEAP_SIZE / MAX_MELD_Q) + 1;
		for (size_t element_index = 0; element_index < heap_size; ++element_index){
			elements.push_back(rand());
			std::shared_ptr<node> new_shptr = heap_part.insert(elements.back());
			shptrs.push_back(new_shptr);
		}
		//change (one change per heap, otherwise shptrs can become invalid)
		size_t changed_element_index = rand() % heap_size + 1;
		int new_value = rand();
		if (new_value < elements[elements.size() - changed_element_index]){
			++dk_quantity;
		} else if (new_value > elements[elements.size() - changed_element_index]){
			++ik_quantity;
		}
		elements[elements.size() - changed_element_index] = new_value;
		heap_part.change_key(shptrs[shptrs.size() - changed_element_index], new_value);

		heap_all.meld(heap_part);
	}

	//sorting
	std::sort(elements.begin(), elements.end());
	for (size_t element_index = 0; element_index < elements.size(); ++element_index){
		int min = heap_all.extract_min(); //exception can be thrown
		if (min != elements[element_index]){
			throw std::logic_error(std::string("Test_system: minimum is wrong"));
		}
	}
	if (heap_all.size() != 0){
		throw std::logic_error(std::string("Test_system: size is wrong"));
	}

	//output statistics
	std::cout << "Size: " << elements.size() << ". Melds: " << heap_quantity
			  << ". DKs: " << dk_quantity << ". IKs: " << ik_quantity << "\n";
}


void binomial_heap_test_caller(size_t test_quantity)
{
	srand(time(NULL));
	for (size_t test_number = 0; test_number < test_quantity; ++test_number){
		binomial_heap_tester();
		std::cout << "Test #" << test_number+1 << " is passed!\n\n";
	}
	std::cout << "All tests are passed!\n";
}

void binomial_heap_test_caller()
{
	binomial_heap_test_caller(TEST_Q);
}
