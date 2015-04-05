#include "geometry.h"

bool equals(double x, double y) {
	return std::abs(x - y) < kEpsilon;
}

Point::Point(Coord _x, Coord _y)
	: x(_x), y(_y) {}

bool Point::operator == (const Point& other) const {
	return x == other.x && y == other.y;
}

PointSet::PointSet(const std::vector<Point>& _points)
	: points(_points) {}

PointSet::PointSet(size_t initial_size)
	: points(initial_size) {}

Vector::Vector(Coord _x, Coord _y)
	: x(_x), y(_y) {}

Vector::Vector(const Point& from, const Point& to)
	: x(to.x - from.x), y(to.y - from.y) {}

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

Vector Vector::getPerpendicular() const {
	return Vector(y, -x);
}

Line::Line(const Point& _p0, const Vector& _v)
	: p0(_p0), v(_v) {
	if (v.x == 0 && v.y == 0) {
		throw std::logic_error("Wrong line coefficients");
	}
	a = v.y;
	b = -v.x;
	c = v.x * p0.y - v.y * p0.x;
}

Line::Line(Coord _a, Coord _b, Coord _c)
	: a(_a), b(_b), c(_c) {
	if (a == 0 && b == 0) {
		throw std::logic_error("Wrong line coefficients");
	}
	v.x = -b;
	v.y = a;
	if (b != 0) {
		p0.x = 0;
		p0.y = -c / b;
	} else {
		p0.x = -c / a;
		p0.y = 0;
	}
}

Line Line::getPerpendicular(const Point& p) const {
	return Line(-b, a, b * p.x - a * p.y);
}

Point Line::getPoint(double t) const {
	return Point(p0.x + v.x * t, p0.y + v.y * t);
}

Coord crossProduct(const Vector& v1, const Vector& v2) {
	return v1.x * v2.y - v1.y * v2.x;
}

Coord dotProduct(const Vector& v1, const Vector& v2) {
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

Circle::Circle(const Point& _c, double _rad)
	: c(_c), rad(_rad) {}

double distance(const Point& p, const Line& l) {
	return std::abs(l.a * p.x + l.b * p.y + l.c) / std::sqrt(std::pow(l.a, 2) + std::pow(l.b, 2));
}

bool intersects(const Line& line, const Circle& circle) {
	double dist = distance(circle.c, line);
	return equals(dist, circle.rad) || dist < circle.rad;
}

std::vector<Line> getTangents(Circle c1, Circle c2) {
	std::vector<Line> tangents;
	c2.c.x -= c1.c.x;
	c2.c.y -= c1.c.y;
	double len = Vector(c2.c.x, c2.c.y).lengthSquared();
	for (int i = -1; i <= 1; i += 2) {
		double c = i * c1.rad;
		for (int j = -1; j <= 1; j += 2) {
			double rad = j * c2.rad - i * c1.rad;
			double discr = len - pow(rad, 2);
//			if (discr < -kEpsilon) {
//				throw std::logic_error("Got negative discriminant");
//			}
			discr = sqrt(abs(discr));
			double a = (rad * c2.c.x - c2.c.y * discr) / len;
			double b = (rad * c2.c.y + c2.c.x * discr) / len;
			tangents.push_back(Line(a, b, c - a * c1.c.x - b * c1.c.y));
		}
	}
	return tangents;
}
