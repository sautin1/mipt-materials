#pragma once

#include <vector>
#include <opencv2/opencv.hpp>

using namespace cv;

struct Vector {
    Vector(int _x, int _y) : x(_x), y(_y) {}

    int x, y;
};


class MotionEstimator {
public:
    MotionEstimator(int _block_size = 16, int step_size = 8)
        : block_size(_block_size), initial_step_size(step_size) {}
    Vector estimate_global(const Mat& image_current, const Mat& image_previous,
                           bool show_vectors = false) const;
    std::vector<Vector> estimate_local(const Mat& image_current, const Mat& image_previous,
                                       bool show_vectors = false) const;
private:
    double calc_distance(const Mat& block_left, const Mat& block_right) const;
    Point find_closest_block(const Mat& image_base, Point block_start, const Mat& image_to_search) const;

    int block_size;
    int initial_step_size;
};
