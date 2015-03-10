#include <algorithm>
#include <fstream>
#include <iostream>
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


class ConvexHull {
public:
	explicit ConvexHull(const PointSet& pointSet, bool is_sorted);
	Polygon getHullPolygon() const;
	std::vector<int> getHullNodesIndices() const;
private:
	const PointSet& pointSet_;
	std::vector<int> points_sorted_;
	std::vector<int> hull_;
};

ConvexHull::ConvexHull(const PointSet& pointSet, bool is_sorted)
	: pointSet_(pointSet) {
	if (pointSet.points.size() < 3) {
		throw std::logic_error("Too few nodes to build a convex hull");
	}
	// fill permutation
	points_sorted_.reserve(pointSet_.points.size());
	for (size_t point_index = 0; point_index < pointSet_.points.size(); ++point_index) {
		points_sorted_.push_back(point_index);
	}
	// sort points by polar angle
	if (!is_sorted) {
		int origin_index = pointSet.getLowestLeft();
		std::sort(points_sorted_.begin(), points_sorted_.end(),
				  PointsIndexPolarAngleAndLengthComparator(pointSet_, origin_index));
	}

	// put first 2 points
	hull_.push_back(points_sorted_[0]);
	hull_.push_back(points_sorted_[1]);
	// build hull
	for (size_t point_index = 2; point_index < points_sorted_.size(); ++point_index) {
		Vector v_new(pointSet_[hull_.back()], pointSet_[points_sorted_[point_index]]);
		Vector v_old(pointSet_[hull_[hull_.size() - 2]], pointSet_[hull_.back()]);
		while (hull_.size() > 1 && !v_new.isClockwiseRotation(v_old)) {
			hull_.pop_back();
			v_new = Vector(pointSet_[hull_.back()], pointSet_[points_sorted_[point_index]]);
			v_old = Vector(pointSet_[hull_[hull_.size() - 2]], pointSet_[hull_.back()]);
		}
		hull_.push_back(points_sorted_[point_index]);
	}
}

Polygon ConvexHull::getHullPolygon() const {
	std::vector<Point> nodes;
	nodes.reserve(hull_.size());
	for (size_t node_index = 0; node_index < hull_.size(); ++node_index) {
		nodes.push_back(pointSet_[hull_[node_index]]);
	}
	return Polygon(nodes);
}

std::vector<int> ConvexHull::getHullNodesIndices() const {
	return hull_;
}



int main() {
	int nail_quantity;
	std::cin >> nail_quantity;
	PointSet pointSet;
	pointSet.points.reserve(nail_quantity);
	for (int nail_index = 0; nail_index < nail_quantity; ++nail_index) {
		int x, y;
		std::cin >> x >> y;
		pointSet.points.push_back(Point(x, y));
	}

	ConvexHull convex_hull(pointSet, false);
	std::vector<int> hull_indices = convex_hull.getHullNodesIndices();
	Polygon hull_polygon = convex_hull.getHullPolygon();
	std::cout << hull_polygon.perimeter() << ' ' << hull_polygon.area() << '\n';
	return 0;
}

