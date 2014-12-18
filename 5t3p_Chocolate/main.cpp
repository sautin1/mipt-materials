#include <iostream>
#include <limits>
#include <unordered_set>
#include <vector>

int mex(const std::vector<int>& numbers) {
	std::unordered_set<int> number_set(numbers.begin(), numbers.end());
	int mex_result;
	for (mex_result = 0; mex_result < std::numeric_limits<int>::max(); ++mex_result) {
		if (number_set.count(mex_result) == 0) {
			break;
		}
	}
	return mex_result;
}

int main() {
	int max_height, max_width, limit;
	std::cin >> max_height >> max_width >> limit;
	std::vector< std::vector<int> > sprague_grundy(max_height, std::vector<int>(max_width, -1));
	for (int height = 1; height <= max_height; ++height) {
		for (int width = 1; width <= max_width; ++width) {
			if ((height == 1 && width <= limit) || (width == 1 && height <= limit)) {
				sprague_grundy[height-1][width-1] = 0;
			} else {
				std::vector<int> game_decomposition_results;
				for (int cut_height = 1; cut_height <= height / 2; ++cut_height) {
					int decomposition_result = sprague_grundy[cut_height-1][width-1] ^ sprague_grundy[height - cut_height - 1][width-1];
					game_decomposition_results.push_back(decomposition_result);
				}
				for (int cut_width = 1; cut_width <= width / 2; ++cut_width) {
					int decomposition_result = sprague_grundy[height-1][cut_width-1] ^ sprague_grundy[height-1][width - cut_width - 1];
					game_decomposition_results.push_back(decomposition_result);
				}
				sprague_grundy[height-1][width-1] = mex(game_decomposition_results);
			}
		}
	}
	if (sprague_grundy.back().back() > 0) {
		std::cout << 1 << "\n";
	} else {
		std::cout << 2 << "\n";
	}
	return 0;
}

