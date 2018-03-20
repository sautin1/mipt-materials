#include "motion_estimator.h"

Vector MotionEstimator::estimate_global(const Mat& image_current, const Mat& image_previous, bool show_vectors) const {
    std::vector<Vector> local_motion_vectors = estimate_local(image_current, image_previous, show_vectors);
    double x_sum, y_sum;
    for (const Vector& motion_vector : local_motion_vectors) {
        x_sum += motion_vector.x;
        y_sum += motion_vector.y;
    }
    x_sum /= local_motion_vectors.size();
    y_sum /= local_motion_vectors.size();
    return Vector(static_cast<int>(std::round(x_sum)), static_cast<int>(std::round(y_sum)));
}

std::vector<Vector> MotionEstimator::estimate_local(const Mat& image_current, const Mat& image_previous,
                                                    bool show_vectors) const {
    std::vector<Vector> motion_vectors;
    Mat image_to_display = image_current.clone();
    motion_vectors.reserve((image_current.rows - block_size) * (image_current.cols - block_size));
    for (int row_start = 0; row_start < image_current.rows - block_size + 1; row_start += block_size) {
        for (int col_start = 0; col_start < image_current.cols - block_size + 1; col_start += block_size) {
            Point block_current_start(col_start, row_start);
            Point block_previous_start = find_closest_block(image_current, block_current_start, image_previous);
            motion_vectors.emplace_back(block_current_start.x - block_previous_start.x,
                                        block_current_start.y - block_previous_start.y);
            if (show_vectors) {
                rectangle(image_to_display,
                          block_current_start,
                          Point(block_current_start.x + block_size, block_current_start.y + block_size),
                          0,
                          2);
                arrowedLine(image_to_display,
                            Point(block_current_start.x + block_size / 2, block_current_start.y + block_size / 2),
                            Point(block_previous_start.x + block_size / 2, block_previous_start.y + block_size / 2),
                            ((row_start + col_start) % 2 == 0) ? 255 : 0,
                            2);
            }
        }
    }
    if (show_vectors) {
        imshow("Image current", image_to_display);
        waitKey(0);
    }
    return motion_vectors;
}

Point MotionEstimator::find_closest_block(const Mat& image_base,
                                          Point block_start,
                                          const Mat& image_to_search) const {
    Mat block = image_base(Rect(block_start.x, block_start.y, block_size, block_size));
    int step_size = initial_step_size;
    double min_distance = 255 * block_size + 1; // (maximal possible value of calc_distance) + 1
    Point point_best(-1, -1);

    while (step_size > 0) {
        for (int delta_row = -step_size; delta_row <= step_size; delta_row += step_size) {
            for (int delta_col = -step_size; delta_col <= step_size; delta_col += step_size) {
                if ((step_size > 1 && delta_row != 0 && delta_col != 0) ||
                        (block_start.x + delta_col < 0) ||
                        (block_start.x + delta_col + block_size - 1 >= image_base.cols) ||
                        (block_start.y + delta_row < 0) ||
                        (block_start.y + delta_row + block_size - 1 >= image_base.rows)) {
                    continue;
                }
                Rect block_search_rect(block_start.x + delta_col, block_start.y + delta_row,
                                       block_size, block_size);
                double distance = calc_distance(block, image_to_search(block_search_rect));
                if (distance < min_distance) {
                    min_distance = distance;
                    point_best = Point(block_start.x + delta_col, block_start.y + delta_row);
                }
            }
        }
        if (point_best == block_start) {
            step_size /= 2;
        } else {
            block_start = point_best;
        }
    }

    return point_best;
}

double MotionEstimator::calc_distance(const Mat& block_left, const Mat& block_right) const {
    return norm(block_left, block_right, NORM_L2);
}
