#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <stdexcept>

struct soundT {
	size_t sound;

	soundT(std::string& soundString)
	{
		// get hash of a string
		soundString += " ";
		size_t octave = soundString[0] - '1';
		switch (soundString[1]) {
			case 'C':
				if (soundString[2] == ' ') sound = 0;
				else if (soundString[2] == '+') sound = 1;
				else {
					--octave;
					sound = 11;
				}
				break;
			case 'D':
				if (soundString[2] == '-') sound = 1;
				else if (soundString[2] == ' ') sound = 2;
				else sound = 3;
				break;
			case 'E':
				if (soundString[2] == '-') sound = 3;
				else if (soundString[2] == ' ') sound = 4;
				else sound = 5;
				break;
			case 'F':
				if (soundString[2] == '-') sound = 4;
				else if (soundString[2] == ' ') sound = 5;
				else sound = 6;
				break;
			case 'G':
				if (soundString[2] == '-') sound = 6;
				else if (soundString[2] == ' ') sound = 7;
				else sound = 8;
				break;
			case 'A':
				if (soundString[2] == '-') sound = 8;
				else if (soundString[2] == ' ') sound = 9;
				else sound = 10;
				break;
			case 'B':
				if (soundString[2] == '-') sound = 10;
				else if (soundString[2] == ' ') sound = 11;
				else {
					++octave;
					sound = 0;
				}
				break;
		}
		sound = octave * 12 + sound;
	}

	bool operator == (const soundT& rightSound) const
	{
		return sound == rightSound.sound;
	}
};

struct melodyT {
	std::vector<soundT> sounds;
};

size_t maxMelodyIntersection(const melodyT& leftMelody, const melodyT& rightMelody)
{
	std::vector<size_t> intersectionCount(leftMelody.sounds.size() + rightMelody.sounds.size(), 0);
	for (size_t leftIndex = 0; leftIndex < leftMelody.sounds.size(); ++leftIndex) {
		for (size_t rightIndex = 0; rightIndex < rightMelody.sounds.size(); ++rightIndex) {
			if (leftMelody.sounds[leftIndex] == rightMelody.sounds[rightIndex]) {
				size_t diffIndex = leftMelody.sounds.size()-1 + rightIndex - leftIndex;
				++intersectionCount[diffIndex];
			}
		}
	}
	size_t maxIntersection = 0;
	for (size_t countIndex = 0; countIndex < intersectionCount.size(); ++countIndex) {
		maxIntersection = std::max(intersectionCount[countIndex], maxIntersection);
	}
	return maxIntersection;
}


int main()
{
	size_t ourMelodySize;
	std::cin >> ourMelodySize;

	/*std::ifstream fin("input.txt");
	fin >> ourMelodySize;*/

	melodyT ourMelody;
	for (size_t soundIndex = 0; soundIndex < ourMelodySize; ++soundIndex) {
		std::string ourMelodySound;
		std::cin >> ourMelodySound;
		//fin >> ourMelodySound;

		ourMelody.sounds.push_back(soundT(ourMelodySound));
	}
	size_t diskVolume;
	std::cin >> diskVolume;
	//fin >> diskVolume;

	std::vector<double> results;
	for (size_t melodyIndex = 0; melodyIndex < diskVolume; ++melodyIndex) {
		melodyT diskMelody;
		size_t melodySize;
		std::cin >> melodySize;
		//fin >> melodySize;

		for (size_t soundIndex = 0; soundIndex < melodySize; ++soundIndex) {
			std::string melodySound;
			std::cin >> melodySound;
			//fin >> melodySound;

			diskMelody.sounds.push_back(soundT(melodySound));
		}
		results.push_back(maxMelodyIntersection(diskMelody, ourMelody)*1.0 / melodySize);
	}
	//fin.close();

	for (size_t resultIndex = 0; resultIndex < results.size(); ++resultIndex) {
		std::cout << results[resultIndex] << "\n";
	}
	return 0;
}
