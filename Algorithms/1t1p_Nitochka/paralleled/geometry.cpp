#include "geometry.h"

Point::Point(int _x, int _y)
	: x(_x), y(_y) {}

bool Point::operator == (const Point& other) const {
	return x == other.x && y == other.y;
}

bool Point::operator != (const Point& other) const {
	return !operator ==(other);
}

PointSet::PointSet(const std::vector<Point>& _points)
	: points(_points) {}

PointSet::PointSet(size_t initial_size)
	: points(initial_size) {}

Vector::Vector(int _x, int _y)
	: x(_x), y(_y) {}

Vector::Vector(const Point& from, const Point& to)
	: x(to.x - from.x), y(to.y - from.y), start_point(from) {}

long long Vector::lengthSquared() const {
	return dotProduct(*this, *this);
}

double Vector::length() const {
	return std::sqrt(lengthSquared());
}

bool Vector::isClockwiseRotation(const Vector& other) const {
	return crossProduct(other, *this) < 0;
}

bool Vector::isParallel(const Vector& other) const {
	return crossProduct(*this, other) == 0;
}

long long crossProduct(const Vector& v1, const Vector& v2) {
	return v1.x * v2.y - v1.y * v2.x;
}

long long dotProduct(const Vector& v1, const Vector& v2) {
	return v1.x * v2.x + v1.y * v2.y;
}

Polygon::Polygon(const std::vector<Point>& nodes)
	: PointSet(nodes), is_counted_area_(false), is_counted_perimeter_(false) {}

size_t Polygon::size() const {
	return points.size();
}

double Polygon::signed_area() {
	if (is_counted_area_) {
		return area_;
	}
	area_ = 0;
	if (size() == 1) {
		return area_;
	}
	Vector v1(points[0], points[1]);
	Vector v2;
	for (size_t node_index = 2; node_index < points.size(); ++node_index) {
		v2 = Vector(points[0], points[node_index]);
		area_ += crossProduct(v1, v2) / 2.0;
		v1 = v2;
	}
	return area_;
}

double Polygon::area() {
	return std::abs(signed_area());
}

double Polygon::perimeter() {
	if (is_counted_perimeter_) {
		return perimeter_;
	}
	perimeter_ = 0;
	for (size_t node_index = 1; node_index < points.size(); ++node_index) {
		Vector v(points[node_index - 1], points[node_index]);
		perimeter_ += v.length();
	}
	perimeter_ += Vector(points.back(), points.front()).length();
	return perimeter_;
}
