#ifndef HASHING_H
#define HASHING_H

#include <stdexcept>
#include <string>
#include <vector>

typedef unsigned long long HashType;

const HashType MODULUS_NEAR_LONG_LONG_MAX = 18446744073709551557U;
const int DEFAULT_PRIME_HORIZONTAL_BASE = 31;
const int DEFAULT_PRIME_VERTICAL_BASE = 37;

class ImmutableCharTableHash {
public:
	ImmutableCharTableHash(const std::vector<std::string>& sample_vector, int vertical_prime_base,
						   int horizontal_prime_base, HashType modulus);

	bool isPalindromicSubtable(int start_vertical_index, int start_horizontal_index,
							   int end_vertical_index, int end_horizontal_index) const;
private:
	typedef std::vector< std::vector<HashType> > HashMatrix;
	HashType countCharHash(int vertical_index, int horizontal_index, char new_char) const;
	int getReversedVerticalIndex(int index) const;
	int getReversedHorizontalIndex(int index) const;
	HashType getSubtableHash(int start_vertical_index, int start_horizontal_index,
							 int end_vertical_index, int end_horizontal_index,
							 const HashMatrix& matrix) const;
	bool isEqualHash(HashType first_hash, int first_start_vertical_index, int first_start_horizontal_index,
					 HashType second_hash, int second_start_vertical_index,
					 int second_start_horizontal_index) const;
	size_t height_, width_;
	std::vector<HashType> horizontal_base_powers_;
	std::vector<HashType> vertical_base_powers_;
	HashType modulus_;
	HashMatrix direct_hash_;
	HashMatrix reverse_hash_;
};

#endif // HASHING_H
