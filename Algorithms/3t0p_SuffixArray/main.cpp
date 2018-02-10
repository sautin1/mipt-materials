#include <iostream>
#include "suffixArray.h"

int main()
{
	std::string str = "ababab";
	SuffixArray suffixArray(str, SuffixArray::SHIFT_ARRAY);
	suffixArray.printLCP(std::cout, "\n");
	return 0;
}
