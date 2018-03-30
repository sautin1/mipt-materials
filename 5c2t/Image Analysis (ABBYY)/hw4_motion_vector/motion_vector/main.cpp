#include <iostream>
#include <string>
#include <utility>
#include <vector>

#include <opencv2/opencv.hpp>
#include "motion_estimator.h"
#include "os.h"

using namespace cv;


std::pair<std::vector<Vec2i>, std::vector<std::string>> detect_motion(const std::vector<std::string>& paths) {
    std::vector<Vec2i> motion_vectors;
    motion_vectors.reserve(paths.size() - 1);
    MotionEstimator estimator(16,   // block_size
                              8,    // step_size
                              0,    // error_weight
                              64,   // disp_weight
                              0.5,  // dev_weight
                              -1,   // belief_threshold
                              80);  // belief_top_percent
    Mat image_first = read_image(paths[0]);
    for (int i = 1; i < paths.size(); ++i) {
        Mat image_previous = read_image(paths[i - 1]);
        Mat image_current = read_image(paths[i]);
        std::pair<Vec2i, std::vector<Vec2i>> result_with_previous = estimator.estimate_global(image_current,
                                                                                              image_previous,
                                                                                              false);
        Vec2i motion_vector = result_with_previous.first;
        if (i > 1) {
            std::pair<Vec2i, std::vector<Vec2i>> result_with_first = estimator.estimate_global(image_current,
                                                                                               image_first,
                                                                                               false);
            if (result_with_first.second.size() < result_with_previous.second.size()) {
                motion_vector = result_with_first.first + motion_vectors.back();
            }
        }
        motion_vectors.push_back(motion_vector);
    }
    return std::make_pair(motion_vectors, paths);
}


int main() {
    std::vector<std::string> paths_images = list_files_in_directory("../data", ".tif", true);
    auto result = detect_motion(paths_images);
    const std::vector<Vec2i>& motion_vectors = result.first;
    const std::vector<std::string>& paths = result.second;

    for (int i = 1; i < paths.size(); ++i) {
        std::cout << paths[1] << std::endl;
        std::cout << paths[i] << std::endl;
        std::cout << motion_vectors[i - 1] << std::endl << std::endl;
    }

    return 0;
}
