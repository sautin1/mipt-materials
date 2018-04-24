#include <iostream>
#include <opencv2/opencv.hpp>
#include <vector>
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
    if (argc == 1) {
        std::cout << "Usage: " << argv[0] << " <image>" << std::endl;
        return 1;
    }
    Mat image = readImage(argv[1]);
    Mat image_bw;
    cvtColor(image, image_bw, COLOR_BGR2GRAY);
    adaptiveThreshold(image_bw, image_bw, 255, CV_ADAPTIVE_THRESH_GAUSSIAN_C, CV_THRESH_BINARY, 51, 0);

    QrDetector detector(3);
    auto res = detector.detect(image_bw);
    for (int i = 0; i < res.first.size(); ++i) {
        Point center = res.first[i];
        int size = res.second[i];
        Point step(size / 2, size / 2);
        rectangle(image, Rect(center - step, center + step), CV_RGB(255, 0, 0), 8);
    }
    namedWindow("image", WINDOW_NORMAL);
    imshow("image", image);
    waitKey(0);

    return 0;
}
