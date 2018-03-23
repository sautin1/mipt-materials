#include "motion_estimator.h"
#include <iostream>

Vec2i MotionEstimator::estimate_global(const Mat& image_current, const Mat& image_previous, bool show_vectors) const {
    Mat local_motion_vectors = estimate_local(image_current, image_previous, show_vectors);
    std::vector<Vec2i> motion_vectors_filtered = filter_by_belief(image_current, image_previous, local_motion_vectors, true);
    double x_mean, y_mean;
    for (const Vec2i& motion_vector : motion_vectors_filtered) {
        x_mean += motion_vector[0];
        y_mean += motion_vector[1];
    }
    x_mean /= motion_vectors_filtered.size();
    y_mean /= motion_vectors_filtered.size();
    return Vec2i(static_cast<int>(std::round(x_mean)), static_cast<int>(std::round(y_mean)));
}

Mat MotionEstimator::estimate_local(const Mat& image_current, const Mat& image_previous, bool show_vectors) const {
    Mat motion_vectors(image_current.rows / block_size, image_current.cols / block_size, CV_8SC2, Vec2i(0, 0));
    Mat image_to_display = image_current.clone();
    for (int row_start = 0; row_start < image_current.rows - block_size + 1; row_start += block_size) {
        for (int col_start = 0; col_start < image_current.cols - block_size + 1; col_start += block_size) {
            Point block_current_start(col_start, row_start);
            Point block_previous_start = find_closest_block(image_current, block_current_start, image_previous);
            motion_vectors.at<Vec2i>(row_start, col_start) = Vec2i(block_current_start.x - block_previous_start.x,
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
        imshow("Local vectors", image_to_display);
        waitKey(0);
    }
    return motion_vectors;
}

double MotionEstimator::calc_distance(const Mat& block_left, const Mat& block_right, NormTypes norm_type) const {
    return norm(block_left, block_right, norm_type);
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
                double distance = calc_distance(block, image_to_search(block_search_rect), NORM_L2);
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

std::vector<Vec2i> MotionEstimator::filter_by_belief(const Mat& image_current,
                                                     const Mat& image_previous,
                                                     const Mat& motion_vectors,
                                                     bool show_vectors) const {
    std::vector<Vec2i> res;
    Mat image_to_display = image_current.clone();
    for (int row_start = 0; row_start < image_current.rows - block_size + 1; row_start += block_size) {
        for (int col_start = 0; col_start < image_current.cols - block_size + 1; col_start += block_size) {
            Point block_start(col_start, row_start);
            std::cout << "belief( " << row_start << " , " << col_start << " ) = ";
            double belief = calc_belief_function(motion_vectors, image_current, image_previous, block_start);
            std::cout << belief << std::endl;
            if (belief >= belief_threshold) {
                res.push_back(motion_vectors.at<Vec2i>(block_start));
                if (show_vectors) {
                    rectangle(image_to_display,
                              block_start,
                              Point(block_start.x + block_size, block_start.y + block_size),
                              255,
                              2);
//                    arrowedLine(image_to_display,
//                                Point(block_start.x + block_size / 2, block_start.y + block_size / 2),
//                                Point(block_previous_start.x + block_size / 2, block_previous_start.y + block_size / 2),
//                                ((row_start + col_start) % 2 == 0) ? 255 : 0,
//                                2);
                }
            }
        }
    }
    if (show_vectors) {
        imshow("Filtered vectors", image_to_display);
        waitKey(0);
    }
    return res;
}

double MotionEstimator::calc_belief_function(const Mat& vectors, const Mat& image_current, const Mat& image_previous,
                                             const Point& block_current_start) const {
    Point motion_vector_point(block_current_start.x / block_size, block_current_start.y / block_size);
    Vec2i motion_vector = vectors.at<Vec2i>(motion_vector_point);
    Rect block_current_rect(block_current_start.x, block_current_start.y, block_size, block_size);
    Rect block_previous_rect(block_current_start.x - motion_vector[0], block_current_start.y - motion_vector[1],
                             block_size, block_size);
    Mat block_current = image_current(block_current_rect);
    Mat block_previous = image_previous(block_previous_rect);
    double error = calc_distance(block_previous, block_current, NORM_L1);

    Scalar block_current_mean, block_current_dev;
    meanStdDev(block_current, block_current_mean, block_current_dev);
    double disp = block_current_dev[0];

    double dev = 0;
    for (int delta_row = -1; delta_row < 2; delta_row += 2) {
        for (int delta_col = -1; delta_col < 2; delta_col += 2) {
            if (motion_vector_point.x + delta_col >= 0 &&
                    motion_vector_point.x + delta_col < image_current.cols / block_size &&
                    motion_vector_point.y + delta_row >= 0 &&
                    motion_vector_point.y + delta_row < image_current.cols / block_size) {
                Vec2i motion_vector_neighbor = vectors.at<Vec2i>(Point(motion_vector_point.x + delta_col,
                                                                       motion_vector_point.y + delta_row));
                dev += calc_distance(Mat(motion_vector, true), Mat(motion_vector_neighbor, true), NORM_L2SQR) / 4;
            }
        }
    }

    return 1.0 / (error_weight * error + disp_weight / (disp * disp) + dev_weight * dev);
}
