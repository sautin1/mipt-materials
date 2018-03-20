#include "motion_estimator.h"

std::vector<Vector> MotionEstimator::estimate_local(const Mat& image_current, const Mat& image_previous) const {
    std::vector<Vector> motion_vectors;
    motion_vectors.reserve((image_previous.rows - block_size) * (image_previous.cols - block_size));
    for (int row_start = 0; row_start < image_previous.rows - block_size; ++row_start) {
        for (int col_start = 0; col_start < image_previous.cols - block_size; ++col_start) {
            Pixel block_start(col_start, row_start);
            Pixel block_closest_start = find_closest_block(image_previous, block_start, image_current);
            motion_vectors.emplace_back(block_start.col - block_closest_start.col,
                                        block_start.row - block_closest_start.row);
        }
    }
    return motion_vectors;
}

Pixel MotionEstimator::find_closest_block(const Mat& image_base,
                                          Pixel block_start,
                                          const Mat& image_to_search) const {
    Mat block = image_base(Rect(block_start.col, block_start.row, block_size, block_size));
    int step_size = initial_step_size;
    double min_distance = 255 * block_size + 1; // (maximal possible value of calc_distance) + 1
    Pixel pixel_best(-1, -1);

    while (step_size > 0) {
        for (int delta_row = -step_size; delta_row <= step_size; delta_row += step_size) {
            for (int delta_col = -step_size; delta_col <= step_size; delta_col += step_size) {
                if ((delta_row != 0 && delta_col != 0) ||
                        (block_start.col + delta_col < 0) ||
                        (block_start.col + delta_col + block_size - 1 >= image_base.cols) ||
                        (block_start.row + delta_row < 0) ||
                        (block_start.row + delta_row + block_size - 1 >= image_base.rows)) {
                    continue;
                }
                Rect block_search_rect(block_start.col + delta_col, block_start.row + delta_row,
                                       block_size, block_size);
                double distance = calc_distance(block, image_to_search(block_search_rect));
                if (distance < min_distance) {
                    min_distance = distance;
                    pixel_best = Pixel(block_start.row + delta_row, block_start.col + delta_col);
                }
            }
        }
        if (pixel_best == block_start) {
            step_size /= 2;
        } else {
            block_start = pixel_best;
        }
    }

    return pixel_best;
}

double MotionEstimator::calc_distance(const Mat& block_left, const Mat& block_right) const {
    return norm(block_left, block_right, NORM_L2);
}
