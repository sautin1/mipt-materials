#pragma once
#include <vector>
#include <opencv2/opencv.hpp>

using namespace cv;


class QrDetector {
public:
    QrDetector(int _row_stride = 1, int _min_center_distance = 10)
        : row_stride(_row_stride), min_center_distance(_min_center_distance) {}
    std::pair<std::vector<Point>, std::vector<int> > detect(const Mat& image) const;

private:
    bool hasFipRatio(const std::vector<int>& state_counts) const;
    Point findFipCenter(const Mat& image, const std::vector<int>& state_counts, int row, int col) const;
    int findFipCenterVertically(const Mat& image, int row_start, int col_center,
                                int count_central, int count_total) const;
    int findFipCenterHorizontally(const Mat& image, int col_start, int row_center,
                                  int count_central, int count_total) const;
    bool hasFipDiagonal(const Mat& image, int row_center, int col_center,
                        int count_central, int count_total) const;
    std::pair<int, int> findClosestPoint(Point point_base, const std::vector<Point>& points) const;

    inline bool isPixelBlack(uchar pixel) const { return pixel < 128; }
    inline bool isPixelWhite(uchar pixel) const { return !isPixelBlack(pixel); }

    const int row_stride;
    const int min_center_distance;
    static const int STATE_COUNT;
    // FIP consists of 5 modules (each module consists of 1 or 3 fractions):
    // black (1), white (1), black (3), white (1), black (1)
    static const int FRACTION_COUNT;
};
