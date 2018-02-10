#include <iostream>
#include <string>
#include <vector>

#include <stdexcept>
#include <string>
#include <vector>

typedef long HashType;

const int DEFAULT_PRIME_HORIZONTAL_BASE = 29;
const int DEFAULT_PRIME_VERTICAL_BASE = 31;

class ImmutableCharTableHash {
public:
	ImmutableCharTableHash(const std::vector<std::string>& sample_vector, int vertical_prime_base,
						   int horizontal_prime_base);

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
	HashMatrix direct_hash_;
	HashMatrix reverse_hash_;
};

ImmutableCharTableHash::ImmutableCharTableHash(const std::vector<std::string>& sample_vector,
											   int vertical_prime_base, int horizontal_prime_base)
	: height_(sample_vector.size()), width_(sample_vector[0].size()), horizontal_base_powers_(sample_vector[0].size(), 0),
	  vertical_base_powers_(sample_vector.size()),
	  direct_hash_(height_, std::vector<HashType>(width_, 0)),
	  reverse_hash_(height_, std::vector<HashType>(width_, 0)) {
	// count prime_base powers
	horizontal_base_powers_[0] = 1;
	for (size_t prime_power = 1; prime_power < width_; ++prime_power) {
		horizontal_base_powers_[prime_power] = horizontal_base_powers_[prime_power - 1] * horizontal_prime_base;
	}
	vertical_base_powers_[0] = 1;
	for (size_t prime_power = 1; prime_power < height_; ++prime_power) {
		vertical_base_powers_[prime_power] = vertical_base_powers_[prime_power - 1] * vertical_prime_base;
	}

	// count hashes
	for (size_t vertical_index = 0; vertical_index < height_; ++vertical_index) {
		for (size_t horizontal_index = 0; horizontal_index < width_; ++horizontal_index) {
			direct_hash_[vertical_index][horizontal_index] = countCharHash(
						vertical_index,
						horizontal_index,
						sample_vector[vertical_index][horizontal_index]);
			if (horizontal_index > 0) {
				direct_hash_[vertical_index][horizontal_index] += direct_hash_[vertical_index][horizontal_index-1];
			}
			if (vertical_index > 0) {
				if (horizontal_index > 0) {
					direct_hash_[vertical_index][horizontal_index] -= direct_hash_[vertical_index-1][horizontal_index-1];
				}
				direct_hash_[vertical_index][horizontal_index] += direct_hash_[vertical_index-1][horizontal_index];
			}

			int horizontal_reverse_index = getReversedHorizontalIndex(horizontal_index);
			int vertical_reverse_index = getReversedVerticalIndex(vertical_index);
			reverse_hash_[vertical_index][horizontal_index] = countCharHash(
						vertical_index,
						horizontal_index,
						sample_vector[vertical_reverse_index][horizontal_reverse_index]);
			if (horizontal_index > 0) {
				reverse_hash_[vertical_index][horizontal_index] += reverse_hash_[vertical_index][horizontal_index-1];
			}
			if (vertical_index > 0) {
				if (horizontal_index > 0) {
					reverse_hash_[vertical_index][horizontal_index] -= reverse_hash_[vertical_index-1][horizontal_index-1];
				}
				reverse_hash_[vertical_index][horizontal_index] += reverse_hash_[vertical_index-1][horizontal_index];
			}
		}
	}
}

bool ImmutableCharTableHash::isPalindromicSubtable(int start_vertical_index, int start_horizontal_index,
												   int end_vertical_index, int end_horizontal_index) const {
	HashType direct_hash = getSubtableHash(start_vertical_index, start_horizontal_index,
										   end_vertical_index, end_horizontal_index, direct_hash_);
	int reverse_start_vertical_index = getReversedVerticalIndex(end_vertical_index);
	int reverse_end_vertical_index = getReversedVerticalIndex(start_vertical_index);
	int reverse_start_horizontal_index = getReversedHorizontalIndex(end_horizontal_index);
	int reverse_end_horizontal_index = getReversedHorizontalIndex(start_horizontal_index);
	HashType reverse_hash = getSubtableHash(reverse_start_vertical_index, reverse_start_horizontal_index,
											reverse_end_vertical_index, reverse_end_horizontal_index, reverse_hash_);
	return isEqualHash(direct_hash, start_vertical_index, start_horizontal_index, reverse_hash,
					   reverse_start_vertical_index, reverse_start_horizontal_index);
}

HashType ImmutableCharTableHash::getSubtableHash(int start_vertical_index, int start_horizontal_index,
												 int end_vertical_index, int end_horizontal_index,
												 const HashMatrix& matrix) const {
	HashType hash = matrix[end_vertical_index][end_horizontal_index];
	if (start_horizontal_index > 0) {
		hash -= matrix[end_vertical_index][start_horizontal_index-1];
	}
	if (start_vertical_index > 0) {
		hash -= matrix[start_vertical_index-1][end_horizontal_index];
		if (start_horizontal_index > 0) {
			hash += matrix[start_vertical_index-1][start_horizontal_index-1];
		}
	}
	return hash;
}

HashType ImmutableCharTableHash::countCharHash(int vertical_index, int horizontal_index, char new_char) const {
	return vertical_base_powers_[vertical_index] * horizontal_base_powers_[horizontal_index] * (new_char - 'a' + 1);
}

int ImmutableCharTableHash::getReversedHorizontalIndex(int index) const {
	return width_ - index - 1;
}

int ImmutableCharTableHash::getReversedVerticalIndex(int index) const {
	return height_ - index - 1;
}

bool ImmutableCharTableHash::isEqualHash(HashType first_hash, int first_start_vertical_index, int first_start_horizontal_index,
										 HashType second_hash, int second_start_vertical_index,
										 int second_start_horizontal_index) const {
	int vertical_index_diff = second_start_vertical_index - first_start_vertical_index;
	if (vertical_index_diff >= 0) {
		first_hash *= vertical_base_powers_[vertical_index_diff];
	} else {
		second_hash *= vertical_base_powers_[-vertical_index_diff];
	}
	int horizontal_index_diff = second_start_horizontal_index - first_start_horizontal_index;
	if (horizontal_index_diff >= 0) {
		first_hash *= horizontal_base_powers_[horizontal_index_diff];
	} else {
		second_hash *= horizontal_base_powers_[-horizontal_index_diff];
	}
	return (first_hash == second_hash);
}


int main()
{
	std::ios_base::sync_with_stdio(false);
	int height, width;
	std::cin >> height >> width;
	std::vector<std::string> string_vector(height, "");
	for (int vertical_index = 0; vertical_index < height; ++vertical_index) {
		string_vector[vertical_index].resize(width + 2);
		for (int horizontal_index = 0; horizontal_index < width; ++horizontal_index) {
			std::cin >> string_vector[vertical_index][horizontal_index];
		}
	}
	ImmutableCharTableHash tableHash(string_vector, DEFAULT_PRIME_VERTICAL_BASE,
									 DEFAULT_PRIME_HORIZONTAL_BASE);
	int counter = 0;
	for (int end_vertical_index = 0; end_vertical_index < height; ++end_vertical_index) {
		for (int end_horizontal_index = 0; end_horizontal_index < width; ++end_horizontal_index) {
			for (int start_vertical_index = 0; start_vertical_index <= end_vertical_index; ++start_vertical_index) {
				for (int start_horizontal_index = 0; start_horizontal_index <= end_horizontal_index; ++start_horizontal_index) {
					if (tableHash.isPalindromicSubtable(start_vertical_index, start_horizontal_index,
														end_vertical_index, end_horizontal_index)) {
						++counter;
					}
				}
			}
		}
	}
	std::cout << counter << "\n";
	return 0;
}

