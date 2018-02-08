#include <iostream>
#include <fstream>
#include <stdexcept>
#include <vector>
#include <string>

const std::string INPUT_FILE  = "lcis.in";
const std::string OUTPUT_FILE = "lcis.out";


void ReadFileToVector(std::ifstream& fin, std::vector<ssize_t>& sequence)
{
	size_t sequence_length;
	fin >> sequence_length;
	sequence.assign(sequence_length, 0);
	for (size_t sequence_index = 0; sequence_index < sequence_length; ++sequence_index){
		fin >> sequence[sequence_index];
	}
}

int main()
{
	std::vector<ssize_t> sequence1;
	std::vector<ssize_t> sequence2;
	std::ifstream fin;
	fin.open(INPUT_FILE.c_str(), std::ifstream::in);
	if (!fin.good()){
		throw std::runtime_error(std::string("Cannot open file ") + INPUT_FILE);
	}
	else {		
		ReadFileToVector(fin, sequence1);
		ReadFileToVector(fin, sequence2);
		fin.close();
	}

	std::vector<size_t> lcis_length(sequence2.size(), 0);
	std::vector<ssize_t> reference(sequence2.size(), -1);

	for (size_t sequence1_index = 0; sequence1_index < sequence1.size(); ++sequence1_index){
		size_t best_lcis = 0;
		ssize_t best_lcis_index = -1;
		for (size_t sequence2_index = 0; sequence2_index < sequence2.size(); ++sequence2_index){
			if (sequence1[sequence1_index] == sequence2[sequence2_index] && lcis_length[sequence2_index] < best_lcis + 1) {
				lcis_length[sequence2_index] = best_lcis + 1;
				reference[sequence2_index] = best_lcis_index;
			} else if (sequence1[sequence1_index] > sequence2[sequence2_index] && lcis_length[sequence2_index] > best_lcis){
				best_lcis = lcis_length[sequence2_index];
				best_lcis_index = sequence2_index;
			}
		}
	}

	ssize_t lcis_end = 0;
	for (size_t sequence2_index = 1; sequence2_index < sequence2.size(); ++sequence2_index){
		if (lcis_length[sequence2_index] > lcis_length[lcis_end]){
			lcis_end = sequence2_index;
		}
	}
	if (lcis_length[lcis_end] == 0){
		lcis_end = -1;
	}

	//BackTrace
	std::vector<ssize_t> lcis;
	size_t lcis_element = lcis_end;
	while (lcis_element != -1){
		lcis.push_back(sequence2[lcis_element]);
		lcis_element = reference[lcis_element];
	}
	
	//Output
	std::ofstream fout;
	fout.open(OUTPUT_FILE.c_str(), std::ofstream::out);
	if (fout.good()){
		fout << lcis.size() << "\n";
		for (ssize_t lcis_index = lcis.size() - 1; lcis_index >= 0; --lcis_index){
			fout << lcis[lcis_index] << " ";
		}
		fout.close();
	} else {
		throw std::runtime_error(std::string("Cannot create file ") + OUTPUT_FILE);
	}

	return 0;
}