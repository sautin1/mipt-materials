#include <iostream>
#include "suffixArray.h"

int main()
{
	std::string str = "abacaba";
	SuffixArray suffixArray(str, SuffixArray::SUFFIX_ARRAY);
	suffixArray.printLCP(std::cout, "\n");
    return 0;
}
