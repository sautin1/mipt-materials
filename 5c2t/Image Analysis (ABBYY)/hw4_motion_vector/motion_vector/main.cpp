#include <iostream>
#include <string>
#include <utility>
#include <vector>

#include <opencv2/opencv.hpp>
#include "motion_estimator.h"
#include "os.h"

using namespace cv;


std::pair<std::vector<Vec2i>, std::vector<std::string>> detect_motion(const std::vector<std::string>& paths,
                                                                      bool verbose = false) {
    std::vector<Vec2i> motion_vectors;
    motion_vectors.reserve(paths.size());
    motion_vectors.push_back(Vec2i(0, 0));
    MotionEstimator estimator(16,   // block_size
                              8,    // step_size
                              0,    // error_weight
                              64,   // disp_weight
                              0.5,  // dev_weight
                              -1,   // belief_threshold
                              70);  // belief_top_percent
    Mat image_first = read_image(paths[0]);
    Mat image_previous = image_first;
    for (int i = 1; i < paths.size(); ++i) {
        Mat image_current = read_image(paths[i]);
        std::vector<Vec2i> local_vectors_previous = estimator.estimate_local_vectors(image_current,
                                                                                     image_previous,
                                                                                     verbose);
        Vec2i motion_vector = estimator.calc_consensus_vector(local_vectors_previous) + motion_vectors.back();
        if (i > 1) {
            std::vector<Vec2i> local_vectors_first = estimator.estimate_local_vectors(image_current,
                                                                                      image_first,
                                                                                      verbose);
            if (estimator.calc_vector_deviation(local_vectors_first) < estimator.calc_vector_deviation(local_vectors_previous)) {
                motion_vector = estimator.calc_consensus_vector(local_vectors_first);
            }
        }
        motion_vectors.push_back(motion_vector);
        image_previous = image_current;
    }
    return std::make_pair(motion_vectors, paths);
}

int main(int argc, char** argv) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " PATH_TO_DIRECTORY [--visualize]" << std::endl;
        return 1;
    }
    std::string path_dir(argv[1]);
    bool visualize = (argc >= 3 && std::string(argv[2]) == "--visualize");
    std::vector<std::string> paths_images = list_files_in_directory(path_dir, ".tif", true);
    auto result = detect_motion(paths_images, visualize);
    const std::vector<Vec2i>& motion_vectors = result.first;
    const std::vector<std::string>& paths = result.second;

    for (int i = 0; i < paths.size(); ++i) {
        std::cout << paths[0] << std::endl;
        std::cout << paths[i] << std::endl;
        std::cout << motion_vectors[i] << std::endl << std::endl;
    }

    return 0;
}
