#include <iostream>
#include <string>
#include <utility>
#include <vector>

#include <opencv2/opencv.hpp>
#include "motion_estimator.h"

using namespace cv;

int main() {
    std::string path_base = "../data/01.tif";
    std::string path_next = "../data/02.tif";

    Mat image_base = cv::imread(path_base, cv::IMREAD_GRAYSCALE);
    if (image_base.data == NULL) {
        throw std::runtime_error("Cannot read image by path: " + path_base);
    }
    Mat image_next = cv::imread(path_next, cv::IMREAD_GRAYSCALE);
    if (image_next.data == NULL) {
        throw std::runtime_error("Cannot read image by path: " + path_next);
    }

    MotionEstimator estimator;
    Vector motion_vector = estimator.estimate_global(image_base, image_next);
    std::cout << motion_vector.x << ' ' << motion_vector.y << std::endl;
    return 0;
}
