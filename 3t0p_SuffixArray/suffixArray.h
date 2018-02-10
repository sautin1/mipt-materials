#ifndef SUFFIXARRAY_H
#define SUFFIXARRAY_H

#include <string>
#include <vector>
#include <unordered_map>
#include <set>
#include <iostream>

class SuffixArray
{
public:
	enum ArrayType {
		SUFFIX_ARRAY, SHIFT_ARRAY
	};
private:
	typedef std::unordered_map<char, size_t> CharCompressionMap;
	std::string sourceString;
	std::vector<size_t> suffixArray;
	std::vector<int> lcp;
	ArrayType arrayType;
	static const char NONEXISTING_CHAR = 1;

	void compressChars(const std::string& string, CharCompressionMap& charMap) const;
	void createSuffixArray();
	void countLCP();
public:
	SuffixArray(const std::string& string, ArrayType type);
	void printSuffixArray(std::ostream& fout, const std::string& delimiter) const;
	void printLCP(std::ostream& fout, const std::string& delimiter) const;
};

#endif // SUFFIXARRAY_H
