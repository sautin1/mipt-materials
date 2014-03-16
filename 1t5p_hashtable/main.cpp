#include <iostream>
#include <string>
#include "hashtable.h"
const size_t bucket_quantity = 2;

int main()
{
	HashTable<std::string, std::string> hashtable(bucket_quantity);
    hashtable.insert(std::string("1"), std::string("one"));
	
	return 0;
}
