#pragma once
#include <algorithm>
#include <cmath>
#include <ctgmath>
#include <set>
#include <stdexcept>
#include <vector>

typedef long long Coord;

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
    Coord lengthSquared() const;
    double length() const;
    bool isClockwiseRotation(const Vector& other) const;
    bool isParallel(const Vector& other) const;
    bool isCodirect(const Vector& other) const;
    Vector getPerpendicular() const;
};

struct Segment: public Vector {
    Point st;
    Segment(const Point& from, const Point& to);
    Segment(const Point& p, const Vector& v);
    Point en() const;
};

struct Line {
    Point p0;
    Vector v;       // p = p0 + v
    Coord a, b, c;  // ax + by + c = 0
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
    explicit Polygon(const std::vector<Point>& nodes);
    size_t size() const;
    double signedArea() const;
    double area() const;
    double perimeter() const;
protected:
    double area_;
    double perimeter_;
private:
    void countArea();
    void countPerimeter();
};

struct ConvexPolygon: public Polygon {
    explicit ConvexPolygon(const std::vector<Point>& nodes);
    void sortNodesCCW();
    void sortNodesCW();
};

class Circle: public Shape {
public:
    Point c;
    double rad;
    Circle(const Point& _c, double _rad);
    double area() const;
    double perimeter() const;
};

double distance(const Point& p, const Line& l);
Point protract(const Point& p, const Vector& v);

bool isOn(const Point& p, const Segment& seg);
bool isOn(const Point& point, const Polygon& poly);
bool isIn(const Point& point, const ConvexPolygon& poly);

std::vector<Point> removeFlatAngles(const std::vector<Point>& points);

ConvexPolygon minkowskiSum(ConvexPolygon p1, ConvexPolygon p2);

bool intersects(const Segment& seg1, const Segment& seg2);
bool intersects(const Segment& seg, const Line& line);
bool intersects(const Line& line, const Circle& circle);
bool intersects(ConvexPolygon p1, const ConvexPolygon& p2);

// returns a pair of intersecting segments or (-1, -1) if there are no any intersections
std::pair<int, int> intersection(const std::vector<Segment>& segs);

std::vector<Line> getTangents(Circle c1, Circle c2);
