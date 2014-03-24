#include <iostream>
#include <string>
#include <ctime>
#include "hashtable.h"
const size_t BUCKET_QUANTITY = 2;
const size_t TEST_QUANTITY = 20;
const std::string ALPHABETH = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

std::string GetRandomString(size_t random_string_size)
{
	std::string random_string;
	for (size_t char_index = 0; char_index < random_string_size; ++char_index){
		size_t new_element_index = rand() % ALPHABETH.length();
		random_string.push_back(ALPHABETH[new_element_index]);
	}
	return random_string;
}

int main()
{
	srand(time(0));
	HashTable<std::string, std::string> hashtable(BUCKET_QUANTITY);

	std::vector<std::string> test_keys;
	std::vector<std::string> test_values;

	//Inserting elements
	std::cout << "\n==Inserting elements==\n";
	for (size_t test_index = 0; test_index < TEST_QUANTITY; ++test_index){
		std::string key = GetRandomString(test_index + 1);
		std::string value = GetRandomString(test_index + 1);
		hashtable.insert(key, value);
		test_keys.push_back(key);
		test_values.push_back(value);
	}

	//Testing find and operator[]
	std::cout << "\n==Finding elements==\n";
	for (size_t test_index = 0; test_index < TEST_QUANTITY; ++test_index){
		HashTable<std::string, std::string>::Iterator element_it = hashtable.find(test_keys[test_index]);
		if (*(element_it) != test_values[test_index]){
			throw std::logic_error(std::string("Find returned false value: ") + test_values[test_index]
								   + " != " + *(element_it));
		}
		if (hashtable[test_keys[test_index]] != test_values[test_index]){
			throw std::logic_error(std::string("Operator[] returned false value: ") + test_values[test_index]
								   + " != " + *(element_it));
		}
	}

	//Testing HashTable::Iterator::operator++
	std::cout << "\n==Writing hashtable contents==\n";
	std::cout << "There are such elements in hashtable: \n";
	size_t counter = 0;
	for (HashTable<std::string, std::string>::Iterator it = hashtable.begin(); it != hashtable.end(); ++it){
		++counter;
		std::cout << counter << ") " << it->first << " : " << it->second << "\n";
	}

	//Testing erase
	std::cout << "\n==Erasing elements==\n";
	for (size_t test_index = 0; test_index < TEST_QUANTITY/2; ++test_index){
		size_t delete_element = rand() % test_keys.size();
		HashTable<std::string, std::string>::Iterator it = hashtable.erase(test_keys[delete_element]);
		std::cout << test_index+1 << ") Erasing element " << test_keys[delete_element] << " : " << test_values[delete_element] << "\n";
		if (it != hashtable.end()){
			std::cout << "The next element after erased is: " << it->first << " : " << it->second << "\n";
		} else {
			std::cout << "No elements after the erased one.\n";
		}
	}

	//Testing HashTable::Iterator::operator--
	std::cout << "\n==Writing hashtable contents==\n";
	std::cout << "There are such elements NOW in hashtable: \n";
	counter = 0;
	HashTable<std::string, std::string>::Iterator hashtable_end = hashtable.end();
	--hashtable_end;
	for (HashTable<std::string, std::string>::Iterator it = hashtable_end; it != hashtable.begin(); --it){
		++counter;
		std::cout << counter << ") " << it->first << " : " << it->second << "\n";
	}
	std::cout << hashtable.begin()->first << " : " << hashtable.begin()->second << "\n\n";

	std::cout << "All tests passed!\n";
	return 0;
}
