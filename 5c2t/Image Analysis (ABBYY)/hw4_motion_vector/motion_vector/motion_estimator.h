#pragma once

#include <vector>
#include <opencv2/opencv.hpp>

using namespace cv;

class MotionEstimator {
public:
    MotionEstimator(int _block_size = 16, int step_size = 8, double _error_weight = 0.25,
                    double _disp_weight = 32, double _dev_weight = 1, double _belief_threshold = 10)
        : block_size(_block_size), initial_step_size(step_size), error_weight(_error_weight),
          disp_weight(_disp_weight), dev_weight(_dev_weight), belief_threshold(_belief_threshold) {}
    Vec2i estimate_global(const Mat& image_current, const Mat& image_previous, bool show_vectors = false) const;
    Mat estimate_local(const Mat& image_current, const Mat& image_previous, bool show_vectors = false) const;
private:
    double calc_distance(const Mat& block_left, const Mat& block_right, NormTypes norm_type) const;
    Point find_closest_block(const Mat& image_base, Point block_start, const Mat& image_to_search) const;
    std::vector<Vec2i> filter_by_belief(const Mat& image_current, const std::vector<Vec2i>& motion_vectors) const;
    double calc_belief_function(const Vec2i& vector, const Mat& block) const;

    const int block_size;
    const int initial_step_size;

    // weights for belief function calculation
    const double error_weight, disp_weight, dev_weight;
    const double belief_threshold;
};
