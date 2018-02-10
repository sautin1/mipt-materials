#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <vector>

typedef double Coord;

const double kEpsilon = 1e-9;

bool equals(double x, double y);

struct Point {
	Coord x;
	Coord y;
	Point();
	Point(Coord _x, Coord _y);
	bool operator == (const Point& other) const;
};

struct PointSet {
	std::vector<Point> points;

	PointSet();
	explicit PointSet(const std::vector<Point>& _points);
	explicit PointSet(size_t initial_size);
};

struct Vector {
	Coord x;
	Coord y;

	Vector();
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
	Polygon();
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

bool equals(double x, double y) {
	return abs(x - y) < kEpsilon;
}

Point::Point() {}

Point::Point(Coord _x, Coord _y)
	: x(_x), y(_y) {}

bool Point::operator == (const Point& other) const {
	return x == other.x && y == other.y;
}

PointSet::PointSet() {}

PointSet::PointSet(const std::vector<Point>& _points)
	: points(_points) {}

PointSet::PointSet(size_t initial_size)
	: points(initial_size) {}

Vector::Vector() {}

Vector::Vector(Coord _x, Coord _y)
	: x(_x), y(_y) {}

Vector::Vector(const Point& from, const Point& to)
	: x(to.x - from.x), y(to.y - from.y) {}

long long Vector::lengthSquared() const {
	return dotProduct(*this, *this);
}

double Vector::length() const {
	return sqrt(1.0 * lengthSquared());
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

Polygon::Polygon() {}

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
	return abs(signed_area());
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
	return abs(l.a * p.x + l.b * p.y + l.c) / sqrt(1.0 * (pow(l.a, 2) + pow(l.b, 2)));
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
//			if (discr < kEpsilon) {
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

int main() {
	std::ios_base::sync_with_stdio(false);
	std::vector<Circle> trees;
	std::ifstream fin("INPUT.TXT");
	size_t tree_quantity;
	fin >> tree_quantity;
	trees.reserve(tree_quantity);
	for (size_t tree_index = 0; tree_index < tree_quantity; ++tree_index) {
		double x, y, radius;
		fin >> x >> y >> radius;
		trees.push_back(Circle(Point(x, y), radius + kEpsilon));
	}
	fin.close();
	bool found_road = false;
	for (size_t i = 0; i < tree_quantity-1; ++i) {
		for (size_t j = i + 1; j < tree_quantity; ++j) {
			std::vector<Line> tangents = getTangents(trees[i], trees[j]);
			for (int tangent_index = 1; tangent_index <= 2; ++tangent_index) {
				found_road = true;
				for (size_t k = 0; k < tree_quantity; ++k) {
					if (k == i || k == j) {
						continue;
					}
					if (intersects(tangents[tangent_index], trees[k])) {
						found_road = false;
						break;
					}
				}
				if (found_road) {
					break;
				}
			}
			if (found_road) {
				break;
			}
		}
		if (found_road) {
			break;
		}
	}

	std::ofstream fout("OUTPUT.TXT");
	if (found_road) {
		fout << "NO\n";
	} else {
		fout << "YES\n";
	}
	fout.close();

	return 0;
}
