#include <vector>
#include <string>
#include <fstream>
#include <stdexcept>
#include <limits>
#include <iostream>
#include <algorithm>

const ssize_t INF = std::numeric_limits<ssize_t>::max();
const std::string INPUT_FILE = "lis.in";
const std::string OUTPUT_FILE = "lis.out";

class Comparator
{
public:
	Comparator(const std::vector<ssize_t>* number_sequence)
	{
		number_sequence_ = number_sequence;
	}

	bool operator()(const ssize_t search_object, const size_t element) const
	{
		return (*number_sequence_)[element] >= search_object;
	}

private:
	const std::vector<ssize_t>* number_sequence_;
};

int main()
{
	std::ifstream fin;
	fin.open(INPUT_FILE.c_str());
	if (!fin.is_open()){
		throw std::runtime_error("Cannot open input file \"" + INPUT_FILE + "\"");
	}
	std::vector<ssize_t> number_sequence;
	number_sequence.push_back(-INF);

	std::vector<size_t> lis_end_element;
	lis_end_element.push_back(0); //zero-length lis ends on number_sequence[0] = -INF
	std::vector<size_t> reference;
	reference.push_back(0);
	size_t new_element_id = number_sequence.size() - 1;
	while (fin.good()){
		ssize_t new_element;
		fin >> new_element;
		number_sequence.push_back(new_element);
		new_element_id ++;

		if (new_element > number_sequence[lis_end_element.back()]){
			reference.push_back(lis_end_element.back());
			lis_end_element.push_back(new_element_id);
		} 
		else {
			Comparator compare_functor(&number_sequence);
			std::vector<size_t>::iterator element_position_it;
			element_position_it = std::upper_bound(lis_end_element.begin(), lis_end_element.end(), new_element, compare_functor);
			size_t element_position = element_position_it - lis_end_element.begin();

			lis_end_element[element_position] = new_element_id;
			reference.push_back(lis_end_element[element_position - 1]);
		}
	}
	fin.close();

	std::vector<size_t> lis;
	size_t lis_element_id = lis_end_element.back();
	while (lis_element_id > 0){
		lis.push_back(lis_element_id);
		lis_element_id = reference[lis_element_id];
	}

	std::ofstream fout;
	fout.open(OUTPUT_FILE.c_str());
	if (!fout.is_open()){
		throw std::runtime_error("Cannot create output file \"" + OUTPUT_FILE + "\"");
	}

	for (ssize_t lis_element = lis.size()-1; lis_element >= 0; lis_element--){
		fout << number_sequence[lis[lis_element]] << " ";
	}
	fout.close();

	return 0;
}