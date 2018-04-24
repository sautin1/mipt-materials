#include <iostream>
#include <opencv2/opencv.hpp>
#include <vector>
#include "timer.h"
#include "qr_detector.h"


using namespace cv;


Mat readImage(const std::string& path, ImreadModes mode = IMREAD_COLOR) {
    Mat image = imread(path, mode);
    if (image.empty()) {
        throw std::runtime_error("Cannot read image by path: " + path);
    }
    return image;
}


int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <image>" << " [-o <output directory>] [--time]" << std::endl;
        return 1;
    }
    std::string path(argv[1]);
    std::string path_output;
    if (argc >= 4 && std::string(argv[2]) == "-o") {
        path_output = std::string(argv[3]);
    }
    bool measure_time = (argc >= 3 && std::string(argv[2]) == "--time")
            || (argc >= 5 && std::string(argv[4]) == "--time");
    Mat image = readImage(path);
    Mat image_bw;
    cvtColor(image, image_bw, COLOR_BGR2GRAY);
    adaptiveThreshold(image_bw, image_bw, 255, CV_ADAPTIVE_THRESH_GAUSSIAN_C, CV_THRESH_BINARY, 51, 0);

    QrDetector detector(3, 10, measure_time);
    Timer timer;
    if (measure_time) {
        timer.start();
    }
    auto res = detector.detect(image_bw);
    if (measure_time) {
        timer.stop();
        std::cout << "Time: " << timer.get_duration() << "ms" << std::endl;
    }

    for (int i = 0; i < res.first.size(); ++i) {
        Point center = res.first[i];
        int size = res.second[i];
        Point step(size / 2, size / 2);
        rectangle(image, Rect(center - step, center + step), CV_RGB(255, 0, 0), 8);
    }
    if (path_output.empty()) {
        namedWindow("image", WINDOW_NORMAL);
        imshow("image", image);
        waitKey(0);
    } else {
        imwrite(path_output + "/bw.png", image_bw);
        imwrite(path_output + "/fips.png", image);
    }

    return 0;
}
