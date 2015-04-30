#ifndef GEOMETRY_H
#define GEOMETRY_H

#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <vector>

typedef double Coord;

const double kEpsilon = 1e-9;
const double kPi = 3.1415926535897932384626433;

bool equals(double x, double y);

struct Point {
    Coord x;
    Coord y;
    Point() = default;
    Point(Coord _x, Coord _y);
    bool operator == (const Point& other) const;
    bool operator < (const Point& other) const;
    Point operator + (const Point& other) const;
    Point operator * (const Coord mult) const;
};

struct PointSet {
    std::vector<Point> points;

    PointSet() = default;
    explicit PointSet(const std::vector<Point>& _points);
    explicit PointSet(size_t initial_size);
};

struct Vector {
    Coord x;
    Coord y;

    Vector() = default;
    Vector(Coord _x, Coord _y);
    Vector(const Point& from, const Point& to);
    long long lengthSquared() const;
    double length() const;
    bool isClockwiseRotation(const Vector& other) const;
    bool isParallel(const Vector& other) const;
    Vector getPerpendicular() const;
};

struct Line {
    Point p0;
    Vector v; // p = p0 + v
    Coord a, b, c; // ax + by + c = 0
    Line(const Point& _p0, const Vector& _v);
    Line(Coord _a, Coord _b, Coord _c);
    Line getPerpendicular(const Point& p) const;
    Point getPoint(double t) const;
};

Coord crossProduct(const Vector& v1, const Vector& v2);
Coord dotProduct(const Vector& v1, const Vector& v2);

class Shape {
public:
    virtual double area() const = 0;
    virtual double perimeter() const = 0;
};

struct Polygon: public PointSet, public Shape {
//  Polygon() = default;
    explicit Polygon(const std::vector<Point>& nodes);
    size_t size() const;
    double signedArea() const;
    double area() const;
    double perimeter() const;
    void sortNodesCCW();
    void sortNodesCW();
protected:
    double area_;
    double perimeter_;
private:
    double countArea();
    double countPerimeter();
};

struct ConvexPolygon: public Polygon {
//  ConvexPolygon() = default;
    explicit ConvexPolygon(const std::vector<Point>& nodes);
};

class Circle: public Shape {
public:
    Point c;
    double rad;
    Circle(const Point& _c, double _rad);
    double area() const;
    double perimeter() const;
};

template <class InputIterator>
ConvexPolygon convexHull(const InputIterator& begin_it, const InputIterator& end_it);

double distance(const Point& p, const Line& l);
Point protract(const Point& p, const Vector& v);

bool isIn(const Point& p, const Vector& v);
bool isIn(const Point& point, const Polygon& poly);

ConvexPolygon minkowskiSum(ConvexPolygon p1, ConvexPolygon p2);

bool intersects(const Line& line, const Circle& circle);
bool intersects(ConvexPolygon p1, const ConvexPolygon& p2);
std::vector<Line> getTangents(Circle c1, Circle c2);

#endif // GEOMETRY_H
