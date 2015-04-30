#include <iostream>
#include <fstream>
#include "geometry.h"

ConvexPolygon readConvexPolygon(std::ifstream& fin) {
    size_t poly_size;
    fin >> poly_size;
    std::vector<Point> poly_points;
    poly_points.reserve(poly_size);
    for (size_t i = 0; i < poly_size; ++i) {
        double x, y;
        fin >> x >> y;
        poly_points.push_back(Point(x, y));
    }
    return ConvexPolygon(poly_points);
}

void printConvexPolygon(const ConvexPolygon& poly) {
    for (size_t i = 0; i < poly.points.size(); ++i) {
        std::cout << poly.points[i].x << " ; " << poly.points[i].y << "\n";
    }
}

int main() {
    std::ifstream fin("test.in");
    ConvexPolygon p1 = readConvexPolygon(fin);
    ConvexPolygon p2 = readConvexPolygon(fin);
//  ConvexPolygon sum = minkowskiSum(p2, p1);
//  printConvexPolygon(sum);
    if (intersects(p1, p2)) {
        std::cout << "YES\n";
    } else {
        std::cout << "NO\n";
    }
}
