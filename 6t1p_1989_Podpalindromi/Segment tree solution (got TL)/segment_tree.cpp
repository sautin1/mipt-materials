#include "segment_tree.h"

void checkIndices(int left_index, int right_index, int max_index) {
   if (left_index < 0 || left_index > right_index || right_index > static_cast<int>(max_index)) {
	   throw new std::runtime_error("Indices are wrong");
   }
}
