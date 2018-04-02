#include "motion_estimator.h"
#include <iostream>

Mat read_image(const std::string& path, ImreadModes mode) {
    Mat image = imread(path, mode);
    if (image.empty()) {
        throw std::runtime_error("Cannot read image by path: " + path);
    }
    return image;
}

Vec2i MotionEstimator::estimate_global_vector(Mat image_current, Mat image_previous, bool show_vectors) const {
    std::vector<Vec2i> local_vectors = estimate_local_vectors(image_current, image_previous, show_vectors);
    return calc_consensus_vector(local_vectors);
}

double MotionEstimator::calc_vector_deviation(const std::vector<Vec2i>& vectors) const {
    std::vector<int> xs(vectors.size()), ys(vectors.size());
    for (int i = 0; i < vectors.size(); ++i) {
        xs[i] = vectors[i][0];
        ys[i] = vectors[i][1];
    }
    Mat mat_xs(xs), mat_ys(ys);
    Scalar xs_mean, ys_mean, xs_std, ys_std;
    meanStdDev(mat_xs, xs_mean, xs_std);
    meanStdDev(mat_ys, ys_mean, ys_std);
    return xs_std[0] * xs_std[0] + ys_std[0] * ys_std[0];
}

Vec2i MotionEstimator::calc_consensus_vector(const std::vector<Vec2i>& motion_vectors) const {
    double x_mean = 0;
    double y_mean = 0;
    for (const Vec2i& motion_vector : motion_vectors) {
        x_mean += motion_vector[0];
        y_mean += motion_vector[1];
    }
    x_mean /= motion_vectors.size();
    y_mean /= motion_vectors.size();
    return Vec2i(static_cast<int>(std::round(x_mean)), static_cast<int>(std::round(y_mean)));
}

std::vector<Vec2i> MotionEstimator::estimate_local_vectors(Mat image_current,
                                                           Mat image_previous,
                                                           bool show_vectors) const {
    Mat local_motion_vectors = estimate_local_vector(image_current, image_previous, show_vectors);
    std::vector<Vec2i> motion_vectors_filtered = filter_by_belief(image_current, image_previous,
                                                                  local_motion_vectors, show_vectors);
    if (motion_vectors_filtered.empty()) {
        throw std::logic_error("No vectors left after filtration");
    }
    return motion_vectors_filtered;
}

Mat MotionEstimator::estimate_local_vector(Mat image_current, Mat image_previous, bool show_vectors) const {
    Mat motion_vectors(image_current.rows / block_size, image_current.cols / block_size, CV_32SC2, Vec2i(0, 0));
    Mat image_to_display = image_current.clone();
    if (show_vectors) {
        cvtColor(image_to_display, image_to_display, CV_GRAY2BGR);
    }
    for (int current_block_row = 0; current_block_row < image_current.rows / block_size; current_block_row += 1) {
        for (int current_block_col = 0; current_block_col < image_current.cols / block_size; current_block_col += 1) {
            Point current_block_indices(current_block_col, current_block_row);
            Point current_block_coords = current_block_indices * block_size;
            Point previous_block_coords = find_closest_block(image_current, current_block_coords, image_previous);
            motion_vectors.at<Vec2i>(current_block_indices) = Vec2i(current_block_coords - previous_block_coords);
            if (show_vectors) {
                rectangle(image_to_display,
                          current_block_coords,
                          Point(current_block_coords.x + block_size, current_block_coords.y + block_size),
                          CV_RGB(255, 255, 255),
                          1);
                arrowedLine(image_to_display,
                            Point(current_block_coords.x + block_size / 2, current_block_coords.y + block_size / 2),
                            Point(previous_block_coords.x + block_size / 2, previous_block_coords.y + block_size / 2),
                            ((current_block_row + current_block_col) % 2 == 0) ? CV_RGB(255, 0, 0) : CV_RGB(0, 255, 0),
                            1);
            }
        }
    }
    if (show_vectors) {
        imshow("Local vectors (current image)", image_to_display);
        waitKey(0);
    }
    return motion_vectors;
}

Point MotionEstimator::find_closest_block(Mat image_base,
                                          Point block_coords,
                                          Mat image_to_search) const {
    Mat block = image_base(Rect(block_coords.x, block_coords.y, block_size, block_size));
    int step_size = initial_step_size;
    double min_distance = 255 * block_size + 1; // (maximal possible value of norm L2) + 1
    Point coords_best(-1, -1);

    while (step_size > 0) {
        for (int delta_row = -step_size; delta_row <= step_size; delta_row += step_size) {
            for (int delta_col = -step_size; delta_col <= step_size; delta_col += step_size) {
                if ((step_size > 1 && delta_row != 0 && delta_col != 0) ||
                        (block_coords.x + delta_col < 0) ||
                        (block_coords.x + delta_col + block_size - 1 >= image_base.cols) ||
                        (block_coords.y + delta_row < 0) ||
                        (block_coords.y + delta_row + block_size - 1 >= image_base.rows)) {
                    continue;
                }
                Point block_search_coords = block_coords + Point(delta_col, delta_row);
                Rect block_search_rect(block_search_coords.x, block_search_coords.y, block_size, block_size);
                double distance = norm(block, image_to_search(block_search_rect), NORM_L2);
                if (distance < min_distance) {
                    min_distance = distance;
                    coords_best = block_search_coords;
                }
            }
        }
        if (coords_best == block_coords) {
            step_size /= 2;
        } else {
            block_coords = coords_best;
        }
    }

    return coords_best;
}

double MotionEstimator::calc_threshold(Mat beliefs) const {
    double belief_min, belief_max;
    cv::minMaxLoc(beliefs, &belief_min, &belief_max);
    return belief_min + belief_top_percent / 100.0 * (belief_max - belief_min);
}

std::vector<Vec2i> MotionEstimator::filter_by_belief(Mat image_current,
                                                     Mat image_previous,
                                                     Mat motion_vectors,
                                                     bool show_vectors) const {
    std::vector<Vec2i> res;
    Mat image_to_display = image_current.clone();
    Mat beliefs(motion_vectors.rows, motion_vectors.cols, CV_64F);
    for (int current_block_row = 0; current_block_row < motion_vectors.rows; ++current_block_row) {
        for (int current_block_col = 0; current_block_col < motion_vectors.cols; ++current_block_col) {
            Point current_block_indices(current_block_col, current_block_row);
            double belief = calc_belief_function(motion_vectors, image_current, image_previous, current_block_indices);
            beliefs.at<double>(current_block_indices) = belief;
        }
    }
    double threshold = (belief_threshold >= 0) ? belief_threshold : calc_threshold(beliefs);

    for (int row = 0; row < beliefs.rows; ++row) {
        for (int col = 0; col < beliefs.cols; ++col) {
            Point indices(col, row);
            if (beliefs.at<double>(indices) >= threshold) {
                Vec2i motion_vector = motion_vectors.at<Vec2i>(indices);
                res.push_back(motion_vector);

                if (show_vectors) {
                    Point coords_current = indices * block_size;
                    rectangle(image_to_display,
                              coords_current,
                              coords_current + Point(block_size, block_size),
                              255, 2);
                    Point coords_previous = coords_current + Point(motion_vector);
                    arrowedLine(image_to_display,
                                coords_current + Point(block_size / 2, block_size / 2),
                                coords_previous + Point(block_size / 2, block_size / 2),
                                255, 1);
                }
            }
        }
    }
    if (show_vectors) {
        imshow("Filtered vectors (current image)", image_to_display);
        waitKey(0);
    }
    return res;
}

double MotionEstimator::calc_belief_function(Mat motion_vectors, Mat image_current, Mat image_previous,
                                             Point current_block_indices) const {
    Point current_block_coords = current_block_indices * block_size;
    Vec2i motion_vector = motion_vectors.at<Vec2i>(current_block_indices);
    Rect current_block_rect(current_block_coords.x, current_block_coords.y, block_size, block_size);
    Rect previous_block_rect(current_block_coords.x - motion_vector[0],
                             current_block_coords.y - motion_vector[1],
                             block_size, block_size);
    Mat current_block = image_current(current_block_rect);
    Mat previous_block = image_previous(previous_block_rect);
    double error = norm(previous_block, current_block, NORM_L1);

    Scalar current_block_mean, current_block_dev;
    meanStdDev(current_block, current_block_mean, current_block_dev);
    double disp = current_block_dev[0];

    double dev = 0;
    for (int delta_row = -1; delta_row <= 1; delta_row += 2) {
        for (int delta_col = -1; delta_col <= 1; delta_col += 2) {
            if (current_block_indices.x + delta_col >= 0 &&
                    current_block_indices.x + delta_col < motion_vectors.cols &&
                    current_block_indices.y + delta_row >= 0 &&
                    current_block_indices.y + delta_row < motion_vectors.rows) {
                Point neighbor_block_indices = current_block_indices + Point(delta_col, delta_row);
                Vec2i motion_vector_neighbor = motion_vectors.at<Vec2i>(neighbor_block_indices);
                dev += norm(Matx<int, 1, 2>(motion_vector[0], motion_vector[1]),
                            Matx<int, 1, 2>(motion_vector_neighbor[0], motion_vector_neighbor[1]),
                            NORM_L2SQR) / 4;
            }
        }
    }
    return 1.0 / (error_weight * error + disp_weight / (disp * disp) + dev_weight * sqrt(dev));
}
