#ifndef GEOMETRY_H
#define GEOMETRY_H

#include <algorithm>
#include <stdexcept>
#include <vector>

struct Point {
	int x;
	int y;

	Point(int _x, int _y);
};

struct PointSet {
	std::vector<Point> points;

	PointSet() = default;
	explicit PointSet(const std::vector<Point>& _points);
	Point operator[] (int index) const;
	Point& operator[] (int index);
	int getLowestLeft() const;
};

struct Vector {
	int x;
	int y;

	Vector() = default;
	Vector(int _x, int _y);
	Vector(const Point& from, const Point& to);
	long long lengthSquared() const;
	double length() const;
	bool isClockwiseRotation(Vector other) const;
	bool isParallel(Vector other) const;
};

struct PointsPolarAngleAndLengthComparator {
	Point origin;
	explicit PointsPolarAngleAndLengthComparator(Point _origin);
	bool operator () (const Point& p1, const Point& p2) const;
};

struct PointsIndexPolarAngleAndLengthComparator {
	int origin;
	const PointSet& pointSet;

	PointsIndexPolarAngleAndLengthComparator(const PointSet& _pointSet, int _origin);
	bool operator () (int i1, int i2) const;
};

long long crossProduct(const Vector& v1, const Vector& v2);
long long dotProduct(const Vector& v1, const Vector& v2);

struct Polygon {
	std::vector<Point> nodes;

	Polygon(std::vector<Point> _nodes);
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
