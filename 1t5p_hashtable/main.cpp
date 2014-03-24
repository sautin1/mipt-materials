#include <iostream>
#include <string>
#include "hashtable.h"
const size_t bucket_quantity = 1;

int main()
{
	HashTable<std::string, std::string> hashtable(bucket_quantity);
    std::string zero("0");
    std::string zero1("zero");
    std::string one("1");
    std::string one1("one");
    std::string two("22");
    std::string two1("two");
    std::string three("333");
    std::string three1("three");
    std::cout << hashtable.insert(zero, zero1).second << "\n";
    std::cout << hashtable.insert(one, one1).second << "\n";
    std::cout << hashtable.insert(two, two1).second << "\n";
    std::cout << hashtable.insert(three, three1).second << "\n"; //Here will be rehash
    std::cout << "Inserting " << hashtable.size() << " elements: Finished!\n";

    HashTable<std::string, std::string>::Iterator element_it = hashtable.find("0");
    std::cout << (*element_it) << "\n";
    std::cout << hashtable[one] << "\n";

    std::cout << "List of all elements: \n";
    for (HashTable<std::string, std::string>::Iterator it = hashtable.begin(); it != hashtable.end(); ++it){
        std::cout << it->first << " : " << it->second << "\n";
    }

    element_it = hashtable.erase(zero);
    std::cout << "After zero goes: " << element_it->first << " - " << element_it->second << "\n";
    std::cout << "CHECKPOINT\n";
    return 0;
}
