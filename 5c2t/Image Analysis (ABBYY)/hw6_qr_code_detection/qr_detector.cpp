#include "qr_detector.h"
#include <algorithm>
#include <numeric>


const int QrDetector::STATE_COUNT = 5;
const int QrDetector::FRACTION_COUNT = 7;

std::pair<std::vector<Point>, std::vector<int>> QrDetector::detect(const Mat& image) const {
    std::vector<Point> fip_centers;
    std::vector<int> fip_sizes;
    for (int row = row_stride - 1; row < image.rows; row += row_stride) {
        int current_state = 0;
        std::vector<int> state_counts(STATE_COUNT, 0);
        const uchar* row_ptr = image.ptr<uchar>(row);

        for (int col = 0; col < image.cols; ++col) {
            if (isPixelBlack(row_ptr[col])) {
                // current: black pixel
                if (current_state % 2 == 1) {
                    // previous: white pixel
                    ++current_state;
                }
                ++state_counts[current_state];
            } else {
                // current: white pixel
                if (current_state == 4) {
                    // maybe found a pattern
                    if (hasFipRatio(state_counts)) {
                        Point center_new = findFipCenter(image, state_counts, row, col);
                        if (center_new.x >= 0 && center_new.y >= 0) {
                            int size_new = std::accumulate(state_counts.begin(), state_counts.end(), 0);
                            auto closest = findClosestPoint(center_new, fip_centers);
                            if (closest.first >= 0 && closest.second < min_center_distance) {
                                int closest_idx = closest.first;
                                fip_centers[closest_idx] = (fip_centers[closest_idx] + center_new) / 2;
                                fip_sizes[closest_idx] = (fip_sizes[closest_idx] + size_new) / 2;
                            } else {
                                fip_centers.push_back(center_new);
                                fip_sizes.push_back(size_new);
                            }
                        }

                        state_counts = std::vector<int>(STATE_COUNT, 0);
                        current_state = 0;
                    } else {
                        state_counts = {state_counts[2], state_counts[3], state_counts[4], 1, 0};
                        current_state = 3;
                    }
                } else if (current_state % 2 == 1) {
                    // previous: white pixel
                    ++state_counts[current_state];
                } else {
                    // previous: black pixel
                    ++current_state;
                    ++state_counts[current_state];
                }
            }
        }
    }
    return std::make_pair(fip_centers, fip_sizes);
}


bool QrDetector::hasFipRatio(const std::vector<int>& state_counts) const {
    int pixel_count_total = std::accumulate(state_counts.begin(), state_counts.end(), 0);
    bool has_zeros = std::find(state_counts.begin(), state_counts.end(), 0) != state_counts.end();
    if (has_zeros || pixel_count_total < FRACTION_COUNT) {
        return false;
    }

    int fraction_width = std::ceil(1.0 * pixel_count_total / FRACTION_COUNT);
    int max_variance = fraction_width / 2;
    bool res = true;
    for (int i = 0; i < state_counts.size(); ++i) {
        int module_width = (i == 2) ? 3 : 1;
        res &= (std::abs(module_width * fraction_width - state_counts[i]) < module_width * max_variance);
    }
    return res;
}


Point QrDetector::findFipCenter(const Mat& image, const std::vector<int>& state_counts, int row, int col) const {
    int state_count_total = std::accumulate(state_counts.begin(), state_counts.end(), 0);
    int center_col = col - state_counts[4] - state_counts[3] - state_counts[2] / 2;
    int center_row = findFipCenterVertically(image, row, center_col, state_counts[2], state_count_total);
    if (center_row < 0) {
        return Point(-1, -1);
    }

    center_col = findFipCenterHorizontally(image, center_col, center_row, state_counts[2], state_count_total);
    if (center_col < 0) {
        return Point(-1, -1);
    }

    bool has_diagonal = hasFipDiagonal(image, center_row, center_col, state_counts[2], state_count_total);
    return has_diagonal ? Point(center_col, center_row) : Point(-1, -1);
}


int QrDetector::findFipCenterVertically(const Mat& image, int row_start, int col_center,
                                           int count_central, int count_total) const {
    std::vector<int> state_counts(STATE_COUNT, 0);
    // go up
    int row = row_start;
    while (row >= 0 && isPixelBlack(image.at<uchar>(row, col_center))) {
        --row;
        ++state_counts[2];
    }
    while (row >= 0 && isPixelWhite(image.at<uchar>(row, col_center)) && state_counts[1] < count_central) {
        --row;
        ++state_counts[1];
    }
    while (row >= 0 && isPixelBlack(image.at<uchar>(row, col_center)) && state_counts[0] < count_central) {
        --row;
        ++state_counts[0];
    }
    if (row < 0 || state_counts[0] >= count_central || state_counts[1] >= count_central) {
        return -1;
    }

    // go down
    row = row_start + 1;
    while (row < image.rows && isPixelBlack(image.at<uchar>(row, col_center))) {
        ++row;
        ++state_counts[2];
    }
    while (row < image.rows && isPixelWhite(image.at<uchar>(row, col_center)) && state_counts[3] < count_central) {
        ++row;
        ++state_counts[3];
    }
    while (row < image.rows && isPixelBlack(image.at<uchar>(row, col_center)) && state_counts[4] < count_central) {
        ++row;
        ++state_counts[4];
    }
    if (row >= image.rows || state_counts[3] >= count_central || state_counts[4] >= count_central) {
        return -1;
    }

    // check and update center
    int count_total_new = std::accumulate(state_counts.begin(), state_counts.end(), 0);
    int center = -1;
    if (std::abs(count_total_new - count_total) * 5 < count_total * 2 && hasFipRatio(state_counts)) {
        center = row - state_counts[4] - state_counts[3] - state_counts[2] / 2;
    }
    return center;
}


int QrDetector::findFipCenterHorizontally(const Mat& image, int col_start, int row_center,
                                             int count_central, int count_total) const {
    std::vector<int> state_counts(STATE_COUNT, 0);
    // go left
    int col = col_start;
    while (col >= 0 && isPixelBlack(image.at<uchar>(row_center, col))) {
        --col;
        ++state_counts[2];
    }
    while (col >= 0 && isPixelWhite(image.at<uchar>(row_center, col)) && state_counts[1] < count_central) {
        --col;
        ++state_counts[1];
    }
    while (col >= 0 && isPixelBlack(image.at<uchar>(row_center, col)) && state_counts[0] < count_central) {
        --col;
        ++state_counts[0];
    }
    if (col < 0 || state_counts[0] >= count_central || state_counts[1] >= count_central) {
        return -1;
    }

    // go right
    col = col_start + 1;
    while (col < image.cols && isPixelBlack(image.at<uchar>(row_center, col))) {
        ++col;
        ++state_counts[2];
    }
    while (col < image.cols && isPixelWhite(image.at<uchar>(row_center, col)) && state_counts[3] < count_central) {
        ++col;
        ++state_counts[3];
    }
    while (col < image.cols && isPixelBlack(image.at<uchar>(row_center, col)) && state_counts[4] < count_central) {
        ++col;
        ++state_counts[4];
    }
    if (col >= image.cols || state_counts[3] >= count_central || state_counts[4] >= count_central) {
        return -1;
    }

    // check and update center
    int count_total_new = std::accumulate(state_counts.begin(), state_counts.end(), 0);
    int center = -1;
    if (std::abs(count_total_new - count_total) * 5 < count_total && hasFipRatio(state_counts)) {
        center = col - state_counts[4] - state_counts[3] - state_counts[2] / 2;
    }
    return center;
}


bool QrDetector::hasFipDiagonal(const Mat& image, int row_center, int col_center,
                                int count_central, int count_total) const {
    std::vector<int> state_counts(STATE_COUNT, 0);
    // go up-left
    int row = row_center;
    int col = col_center;
    while (row >= 0 && col >= 0 && isPixelBlack(image.at<uchar>(row, col))) {
        --row; --col;
        ++state_counts[2];
    }
    while (row >= 0 && col >= 0 && isPixelWhite(image.at<uchar>(row, col)) && state_counts[1] < count_central) {
        --row; --col;
        ++state_counts[1];
    }
    while (row >= 0 && col >= 0 && isPixelBlack(image.at<uchar>(row, col)) && state_counts[0] < count_central) {
        --row; --col;
        ++state_counts[0];
    }
    if (row < 0 || col < 0 || state_counts[0] >= count_central || state_counts[1] >= count_central) {
        return false;
    }

    // go down-right
    row = row_center + 1;
    col = col_center + 1;
    while (row < image.rows && col < image.cols && isPixelBlack(image.at<uchar>(row, col))) {
        ++row; ++col;
        ++state_counts[2];
    }
    while (row < image.rows && col < image.cols && isPixelWhite(image.at<uchar>(row, col))
           && state_counts[3] < count_central) {
        ++row; ++col;
        ++state_counts[3];
    }
    while (row < image.rows && col < image.cols && isPixelBlack(image.at<uchar>(row, col))
           && state_counts[4] < count_central) {
        ++row; ++col;
        ++state_counts[4];
    }
    if (row >= image.rows || col >= image.cols || state_counts[3] >= count_central || state_counts[4] >= count_central) {
        return false;
    }

    // check
    int count_total_new = std::accumulate(state_counts.begin(), state_counts.end(), 0);
    return std::abs(count_total_new - count_total) < count_total * 2 && hasFipRatio(state_counts);
}


std::pair<int, int> QrDetector::findClosestPoint(Point point_base, const std::vector<Point>& points) const {
    int closest_point_idx = -1;
    double closest_point_distance = -1;
    for (int i = 0; i < points.size(); ++i) {
        double distance = norm(point_base - points[i]);
        if (closest_point_distance < 0 || distance < closest_point_distance) {
            closest_point_distance = distance;
            closest_point_idx = i;
        }
    }
    return std::make_pair(closest_point_idx, closest_point_distance);
}
