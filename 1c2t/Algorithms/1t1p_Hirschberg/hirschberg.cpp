#include <iostream>
#include <fstream>
#include <ios>
#include <stdexcept>
#include <vector>
#include <string>
#include <algorithm>

ssize_t SubstringSize(ssize_t begin_index, ssize_t end_index)
{
	return end_index - begin_index + 1;
}


ssize_t GetOptimalDecomposition(const std::string& s1, const std::string& s2, ssize_t s1_begin_index, ssize_t s1_end_index, 
				ssize_t s2_begin_index, ssize_t s2_end_index, ssize_t& min_distance)
{
	ssize_t s1_size = SubstringSize(s1_begin_index, s1_end_index);
	ssize_t s2_size = SubstringSize(s2_begin_index, s2_end_index);

	std::vector<ssize_t> str_distance; //straight pass edit-distance values
	for (ssize_t i = 0; i <= s2_size; i++){
		str_distance.push_back(i);
	}
	//Straight pass
	ssize_t prev;
	ssize_t cur;
	for (ssize_t i = 0; i < s1_size/2; i++){
		prev = str_distance[0];
		str_distance[0] += 1;
		for (ssize_t j = 0; j < s2_size; j++){
			cur = str_distance[j+1];

			if (s1[s1_begin_index + i] == s2[s2_begin_index + j]){
				str_distance[j+1] = prev;
			}
			else{
				str_distance[j+1] = 1 + std::min(str_distance[j+1], std::min(prev, str_distance[j]));
			}
			prev = cur;
		}
	}

	//Reverse pass
	std::vector<ssize_t> rev_distance; //reverse pass edit-distance values
	rev_distance.push_back(0);
	for (ssize_t i = 0; i < s2_size; i++){
		rev_distance.push_back(i + 1);
	}
	for (ssize_t i = 0; i < s1_size - s1_size/2; i++){
		prev = rev_distance[0];
		rev_distance[0] += 1;
		for (ssize_t j = 0; j < s2_size; j++){
			cur = rev_distance[j+1];

			if (s1[s1_end_index - i] == s2[s2_end_index - j]){
				rev_distance[j+1] = prev;
			}
			else{
				rev_distance[j+1] = 1 + std::min(rev_distance[j+1], std::min(prev, rev_distance[j]));
			}
			prev = cur;
		}
	}

	//Find min_row
	ssize_t min_row = 0;
	for (ssize_t i = 1; i < str_distance.size(); i++){
		if (str_distance[i]       + rev_distance[str_distance.size() - i - 1] < 
			str_distance[min_row] + rev_distance[str_distance.size() - min_row - 1]){
			min_row = i;
		}
	}
	min_distance = str_distance[min_row] + rev_distance[str_distance.size() - min_row - 1];
	return min_row;
}


ssize_t Hirschberg(const std::string& s1, const std::string& s2, ssize_t s1_begin_index, ssize_t s1_end_index, 
				ssize_t s2_begin_index, ssize_t s2_end_index, std::ofstream& fout)
{
	ssize_t s1_size = SubstringSize(s1_begin_index, s1_end_index);
	ssize_t s2_size = SubstringSize(s2_begin_index, s2_end_index);
	
	if (s1_size == 0){
		for (ssize_t i = s2_begin_index; i <= s2_end_index; i++){
			fout << "Position " << s1_begin_index << ": Add \"" << s2[i] << "\"\n";
		}
		return s2_size;
	}
	else if (s1_size == 1){
		if (s2_size == 0){
			fout << "Position " << s1_begin_index << ": Delete \"" << s1[s1_begin_index] << "\"\n";
			return 1;
		}
		else if (s2_size == 1){
			if (s1[s1_begin_index] != s2[s2_begin_index]){
				fout << "Position " << s1_begin_index << ": Change \"" << s1[s1_begin_index] << "\" to \"" << s2[s2_begin_index] << "\"\n";
				return 1;
			}
			else{
				return 0;
			}
		}
		else if (s2_size > 1){
			ssize_t s1_letter_used = 0;
			for (ssize_t i = s2_begin_index; i < s2_end_index; i++){
				if (s1[s1_begin_index] != s2[i] || s1_letter_used) {
					fout << "Position " << s1_begin_index + s1_letter_used << ": Add \"" << s2[i] << "\"\n";
				}
				else{
					s1_letter_used = 1;
				}
			}
			//Process the last symbol of s1
			if (s1_letter_used){
				fout << "Position " << s1_begin_index + s1_letter_used << ": Add \"" << s2[s2_end_index] << "\"\n";
				return s2_size - 1;
			}
			else{
				if (s2[s2_end_index] != s1[s1_begin_index]){
					fout << "Position " << s1_begin_index << ": Change \"" << s1[s1_begin_index] << "\" to \"" << s2[s2_end_index] << "\"\n";
					return s2_size;
				}
				else{
					return s2_size - 1;
				}
			}
		}
	}
	else if (s1_size > 1){
		if (s2_size == 0){
			for (ssize_t i = s1_begin_index; i <= s1_end_index; i++){
				fout << "Position " << i << ": Delete \"" << s1[i] << "\"\n";
			}
			return s1_size;
		}
		else if (s2_size == 1){
			ssize_t s2_letter_used = 0;
			for (ssize_t i = s1_begin_index; i < s1_end_index; i++){
				if (s2[s2_begin_index] != s1[i] || s2_letter_used) {
					fout << "Position " << i << ": Delete \"" << s1[i] << "\"\n";
				}
				else{
					s2_letter_used = 1;
				}
			}
			//Process the last symbol of s1
			if (s2_letter_used){
				fout << "Position " << s1_end_index << ": Delete \"" << s1[s1_end_index] << "\"\n";
				return s1_size - 1;
			}
			else{
				if (s1[s1_end_index] != s2[s2_begin_index]){
					fout << "Position " << s1_end_index << ": Change \"" << s1[s1_end_index] << "\" to \"" << s2[s2_begin_index] << "\"\n";
					return s1_size;
				}
				else{
					return s1_size - 1;
				}
			}
		}	
	}

	ssize_t min_distance;
	ssize_t min_row = GetOptimalDecomposition(s1, s2, s1_begin_index, s1_end_index, s2_begin_index, s2_end_index, min_distance);

	Hirschberg(s1, s2, s1_begin_index, s1_begin_index + s1_size/2 - 1, s2_begin_index, s2_begin_index + min_row-1, fout);
	Hirschberg(s1, s2, s1_begin_index + s1_size/2, s1_end_index, s2_begin_index + min_row, s2_end_index, fout);
	return min_distance;
}

void GetFileToString(std::string& result_string, const char* filename)
{
	std::ifstream fin;
	fin.open(filename);
	if (fin.good()) {
		std::getline(fin, result_string, '\0');
		fin.close();
	}
	else {
		throw std::runtime_error(std::string("Cannot open file ") + filename);
	}
}


int main(int argc, char const *argv[])
{
	//Check parameters
	if (argc != 3) {
		throw std::runtime_error("Wrong number of arguments: \nCall: ./hirschberg filename1 filename2");
	}
	//Read two strings
	std::string s1;
	std::string s2;
	GetFileToString(s1, argv[1]);
	GetFileToString(s2, argv[2]);

	std::ofstream fout;
	fout.open("hirschberg_result.res");
	if (fout.good()) {
		ssize_t edit_distance = Hirschberg(s1, s2, 0, s1.size() - 1, 0, s2.size() - 1, fout);
		fout << edit_distance;
	}
	else {
		throw std::runtime_error("Cannot create result file");
	}

	fout.close();
	return 0;
}