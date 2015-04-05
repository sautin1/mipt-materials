#ifndef GEOMETRY_H
#define GEOMETRY_H

#include <algorithm>
#include <stdexcept>
#include <vector>

typedef double Coord;

const double kEpsilon = 1e-9;

bool equals(double x, double y);

struct Point {
	Coord x;
	Coord y;
	Point() = default;
	Point(Coord _x, Coord _y);
	bool operator == (const Point& other) const;
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

struct Polygon: public PointSet {
	Polygon() = default;
	explicit Polygon(const std::vector<Point>& nodes);
	size_t size() const;
	double signed_area();
	double area();
	double perimeter();
private:
	double area_;
	double perimeter_;
	bool is_counted_area_;
	bool is_counted_perimeter_;
};

struct Circle {
	Point c;
	double rad;
	Circle(const Point& _c, double _rad);
};

double distance(const Point& p, const Line& l);
bool intersects(const Line& line, const Circle& circle);
std::vector<Line> getTangents(Circle c1, Circle c2);

#endif // GEOMETRY_H
