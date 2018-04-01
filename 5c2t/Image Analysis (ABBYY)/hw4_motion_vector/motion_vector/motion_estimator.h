#pragma once

#include <vector>
#include <opencv2/opencv.hpp>

using namespace cv;


Mat read_image(const std::string& path, ImreadModes mode = IMREAD_GRAYSCALE);


class MotionEstimator {
public:
    MotionEstimator(int _block_size = 16, int step_size = 8, double _error_weight = 0.25,
                    double _disp_weight = 32, double _dev_weight = 1,
                    double _belief_threshold = 3e-3, int _belief_top_percent = 30)
        : block_size(_block_size), initial_step_size(step_size), error_weight(_error_weight),
          disp_weight(_disp_weight), dev_weight(_dev_weight),
          belief_threshold(_belief_threshold), belief_top_percent(_belief_top_percent) {}
    Vec2i estimate_global_vector(Mat image_current, Mat image_previous, bool show_vectors = false) const;
    double calc_vector_deviation(const std::vector<Vec2i>& vectors) const;
    Vec2i calc_consensus_vector(const std::vector<Vec2i>& motion_vectors) const;
    std::vector<Vec2i> estimate_local_vectors(Mat image_current, Mat image_previous, bool show_vectors = false) const;
    Mat estimate_local_vector(Mat image_current, Mat image_previous, bool show_vectors = false) const;
private:
    Point find_closest_block(Mat image_base, Point block_coords, Mat image_to_search) const;
    double calc_threshold(Mat beliefs) const;
    std::vector<Vec2i> filter_by_belief(Mat image_current, Mat image_previous,
                                        Mat motion_vectors, bool show_vectors) const;
    double calc_belief_function(Mat motion_vectors, Mat image_current, Mat image_previous, Point current_block_indices) const;

    const int block_size;
    const int initial_step_size;

    // weights for belief function calculation
    const double error_weight, disp_weight, dev_weight;
    const double belief_threshold;
    const int belief_top_percent;
};
