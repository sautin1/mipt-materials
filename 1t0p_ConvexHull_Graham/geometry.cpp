#include "geometry.h"

Point::Point(int _x, int _y)
	: x(_x), y(_y) {}

PointSet::PointSet(const std::vector<Point>& _points)
	: points(_points) {}

int PointSet::getLowestLeft() const {
	int lowest_point_index = 0;
	for (size_t point_index = 0; point_index < points.size(); ++point_index) {
		if ((points[point_index].y == points[lowest_point_index].y
			 && points[point_index].x < points[lowest_point_index].x)
				|| points[point_index].y < points[lowest_point_index].y) {
			lowest_point_index = point_index;
		}
	}
	return lowest_point_index;
}

Point PointSet::operator[] (int index) const {
	return points[index];
}

Point& PointSet::operator[] (int index) {
	return points[index];
}

Vector::Vector(int _x, int _y)
	: x(_x), y(_y) {}

Vector::Vector(const Point& from, const Point& to)
	: x(to.x - from.x), y(to.y - from.y) {}

long long Vector::lengthSquared() const {
	return dotProduct(*this, *this);
}

double Vector::length() const {
	return std::sqrt(lengthSquared());
}

bool Vector::isClockwiseRotation(Vector other) const {
	return crossProduct(other, *this) < 0;
}

bool Vector::isParallel(Vector other) const {
	return crossProduct(*this, other) == 0;
}

PointsPolarAngleAndLengthComparator::PointsPolarAngleAndLengthComparator(Point _origin)
	: origin(_origin) {}

bool PointsPolarAngleAndLengthComparator::operator () (const Point& p1, const Point& p2) const {
	Vector v1(origin, p1);
	Vector v2(origin, p2);
	return (v2.isClockwiseRotation(v1) || (v1.isParallel(v2) && v1.lengthSquared() < v2.lengthSquared()));
}

PointsIndexPolarAngleAndLengthComparator::PointsIndexPolarAngleAndLengthComparator(const PointSet& _pointSet, int _origin)
	: origin(_origin), pointSet(_pointSet) {}

bool PointsIndexPolarAngleAndLengthComparator::operator () (int i1, int i2) const {
	const Point& p1 = pointSet[i1];
	const Point& p2 = pointSet[i2];
	return PointsPolarAngleAndLengthComparator(pointSet[origin])(p1, p2);
}

long long crossProduct(const Vector& v1, const Vector& v2) {
	return v1.x * v2.y - v1.y * v2.x;
}

long long dotProduct(const Vector& v1, const Vector& v2) {
	return v1.x * v2.x + v1.y * v2.y;
}

Polygon::Polygon(std::vector<Point> _nodes)
	: nodes(_nodes), is_counted_area_(false), is_counted_perimeter_(false) {
	if (_nodes.size() < 3) {
		throw std::logic_error("Too few nodes to build a polygon");
	}
}

double Polygon::signed_area() {
	if (is_counted_area_) {
		return area_;
	}
	area_ = 0;
	Vector v1 = Vector(nodes[0], nodes[1]);
	Vector v2;
	for (size_t node_index = 2; node_index < nodes.size(); ++node_index) {
		v2 = Vector(nodes[0], nodes[node_index]);
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
	for (size_t node_index = 1; node_index < nodes.size(); ++node_index) {
		Vector v(nodes[node_index - 1], nodes[node_index]);
		perimeter_ += v.length();
	}
	perimeter_ += Vector(nodes.back(), nodes.front()).length();
	return perimeter_;
}
