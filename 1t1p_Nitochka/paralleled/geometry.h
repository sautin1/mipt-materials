#ifndef GEOMETRY_H
#define GEOMETRY_H

#include <algorithm>
#include <stdexcept>
#include <vector>
#include <omp.h>

struct Point {
	int x;
	int y;
	Point() = default;
	Point(int _x, int _y);
	bool operator == (const Point& other) const;
};

struct PointSet {
	std::vector<Point> points;

	PointSet() = default;
	explicit PointSet(const std::vector<Point>& _points);
	explicit PointSet(size_t initial_size);
};

struct Vector {
	int x;
	int y;
	Point start_point;

	Vector() = default;
	Vector(int _x, int _y);
	Vector(const Point& from, const Point& to);
	long long lengthSquared() const;
	double length() const;
	bool isClockwiseRotation(const Vector& other) const;
	bool isParallel(const Vector& other) const;
};

long long crossProduct(const Vector& v1, const Vector& v2);
long long dotProduct(const Vector& v1, const Vector& v2);

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

#endif // GEOMETRY_H
