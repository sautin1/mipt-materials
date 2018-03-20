#include <iostream>
#include <string>
#include <utility>
#include <vector>

#include <opencv2/opencv.hpp>
#include "motion_estimator.h"

using namespace cv;

int main() {
    std::string path_previous = "../data/01.tif";
    std::string path_current = "../data/02.tif";

    Mat image_previous = cv::imread(path_previous, cv::IMREAD_GRAYSCALE);
    if (image_previous.data == NULL) {
        throw std::runtime_error("Cannot read image by path: " + path_previous);
    }
    Mat image_current = cv::imread(path_current, cv::IMREAD_GRAYSCALE);
    if (image_current.data == NULL) {
        throw std::runtime_error("Cannot read image by path: " + path_current);
    }

    MotionEstimator estimator;
    Vec2i motion_vector = estimator.estimate_global(image_current, image_previous, true);
    std::cout << motion_vector[0] << ' ' << motion_vector[1] << std::endl;
    return 0;
}
