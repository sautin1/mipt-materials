#include <fstream>
#include <iostream>
#include <algorithm>
#include <functional>
#include <stdexcept>
#include <vector>

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


Point::Point(int _x, int _y)
	: x(_x), y(_y) {}

bool Point::operator == (const Point& other) const {
	return x == other.x && y == other.y;
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


// splits the set of points into two sets not changing their relative ordering.
// The first set contains points that are located higher or on the vector.
// The second set contains points that are located lower or on the vector.
void splitPointSetByVector(const PointSet& point_set, const Vector& secant,
						   PointSet& top_points, PointSet& low_points);

// sorts points in the set by polar angle.
void sortPolarAngle(PointSet& point_set, const Point& origin);

// takes set of points, sorted by polar angle, and builds convex hull on them.
template <class InputIterator>
Polygon convexHull(const InputIterator& begin_it, const InputIterator& end_it);

// returns area of a convex hull of minimal area on all points from point_set except one.
// First and last points are not removed.
double minAreaOfHullWithInternalNodeRemoved(const PointSet& point_set, Polygon& hull);

// returns area of a convex hull of minimal area on all points from point_set except one.
double minAreaOfHullWithNodeRemoved(const PointSet& point_set);

struct PointComparatorPolarAngleAndLength {
	Point origin;
	explicit PointComparatorPolarAngleAndLength(Point _origin);
	bool operator () (const Point& p1, const Point& p2) const;
};

struct PointComparatorLeftLow {
	bool operator () (const Point& p1, const Point& p2) const;
};

void splitPointSetByVector(const PointSet& point_set, const Vector& secant,
						   PointSet& top_points, PointSet& low_points) {
	for (size_t point_index = 0; point_index < point_set.points.size(); ++point_index) {
		Vector to_point(secant.start_point, point_set.points[point_index]);
		if (secant.isClockwiseRotation(to_point) || secant.isParallel(to_point)) {
			top_points.points.push_back(point_set.points[point_index]);
		}
		if (!secant.isClockwiseRotation(to_point)) {
			low_points.points.push_back(point_set.points[point_index]);
		}
	}
	/* //just some fun with lambdas
	auto lambda_predicate_top = [&secant](const Point& p) -> bool {
		Vector v(secant.start_point, p);
		return secant.isClockwiseRotation(v) || secant.isParallel(v);
	};
	auto lambda_predicate_low = [&secant](const Point& p) -> bool {
		Vector v(secant.start_point, p);
		return !secant.isClockwiseRotation(v);
	};
	std::remove_copy_if(point_set.points.begin(), point_set.points.end(),
						top_points.points.begin(), lambda_predicate_top);
	std::remove_copy_if(point_set.points.begin(), point_set.points.end(),
						top_points.points.begin(), lambda_predicate_top);*/
}

void sortPolarAngle(PointSet& point_set, const Point& origin) {
	std::sort(point_set.points.begin(), point_set.points.end(),
			  PointComparatorPolarAngleAndLength(origin));
}

void sortPolarAngle(PointSet& point_set) {
	Point origin = *(std::min_element(point_set.points.begin(), point_set.points.end(),
									  PointComparatorLeftLow()));
	sortPolarAngle(point_set, origin);
}

template <class InputIterator>
Polygon convexHull(const InputIterator& begin_it, const InputIterator& end_it) {
	std::vector<Point> hull;
	if (end_it - begin_it == 0) {
		return Polygon();
	}
	hull.push_back(*begin_it);
	if (end_it - begin_it == 1) {
		return Polygon(hull);
	}
	hull.push_back(*(begin_it+1));
	Vector v_old(hull[0], hull[1]);
	for (auto point_it = begin_it + 2; point_it != end_it; ++point_it) {
		Vector v_new(hull.back(), *point_it);
		while (hull.size() > 1 && !v_new.isClockwiseRotation(v_old)) {
			hull.pop_back();
			v_new = Vector(hull.back(), *point_it);
			v_old = Vector(hull[hull.size() - 2], hull.back());
		}
		hull.push_back(*point_it);
		v_old = v_new;
	}
	return Polygon(hull);
}

double minAreaOfHullWithInternalNodeRemoved(const PointSet& point_set, Polygon& hull) {
	double min_area = 0;
	auto prev_point_it = point_set.points.begin();
	auto remove_point_it = std::find(point_set.points.begin(), point_set.points.end(),
									 hull.points[1]);
	for (size_t node_index = 1; node_index < hull.size() - 1; ++node_index) {
		// node_index is the point we want to remove
		auto next_point_it = std::find(remove_point_it, point_set.points.end(),
									   hull.points[node_index + 1]);
		Polygon triangle(std::vector<Point> {*prev_point_it, *remove_point_it, *next_point_it});
		if (node_index == 1) {
			min_area = triangle.area();
		}
		PointSet triangle_point_set(next_point_it - prev_point_it);
		auto last_triangle_point_it = std::copy(prev_point_it, remove_point_it,
												triangle_point_set.points.begin());
		std::copy(remove_point_it + 1, next_point_it + 1, last_triangle_point_it);
		Polygon new_hull = convexHull(triangle_point_set.points.begin(),
									  triangle_point_set.points.end());
		if (new_hull.area() - triangle.area() < min_area) {
			min_area = new_hull.area() - triangle.area();
		}
		prev_point_it = remove_point_it;
		remove_point_it = next_point_it;
	}
	return min_area + hull.area();
}

Polygon minAreaTriangularHullWithNodeRemoved(const Polygon& triangle, PointSet& point_set) {
	sortPolarAngle(point_set, triangle.points[0]);
	auto prev_point_it = std::find(point_set.points.begin(),
								   point_set.points.end(),
								   triangle.points[0]);
	auto remove_point_it = std::find(prev_point_it,
									 point_set.points.end(),
									 triangle.points[1]);
	auto next_point_it = std::find(remove_point_it,
								   point_set.points.end(),
								   triangle.points[2]);
	PointSet triangle_point_set(next_point_it - prev_point_it);
	auto last_triangle_point_it = std::copy(prev_point_it, remove_point_it,
											triangle_point_set.points.begin());
	std::copy(remove_point_it + 1, next_point_it + 1, last_triangle_point_it);
	return convexHull(triangle_point_set.points.begin(),
					  triangle_point_set.points.end());
}

double minAreaOfHullWithNodeRemoved(const PointSet& point_set) {
	auto min_max_points = std::minmax_element(point_set.points.begin(), point_set.points.end(),
											  PointComparatorLeftLow());
	Point left_low = *min_max_points.first;
	Point right_top = *min_max_points.second;
	Vector secant(left_low, right_top);
	PointSet top_points;
	PointSet low_points;
	splitPointSetByVector(point_set, secant, top_points, low_points);
	sortPolarAngle(top_points, left_low);
	sortPolarAngle(low_points, right_top);
	Polygon top_hull = convexHull(top_points.points.begin(), top_points.points.end());
	Polygon low_hull = convexHull(low_points.points.begin(), low_points.points.end());
	double total_area = top_hull.area() + low_hull.area();
	double min_top_hull_area = minAreaOfHullWithInternalNodeRemoved(top_points, top_hull);
	double min_low_hull_area = minAreaOfHullWithInternalNodeRemoved(low_points, low_hull);
	double min_area = std::min(min_top_hull_area + low_hull.area(),
							   min_low_hull_area + top_hull.area());

	// remove left_low point
	PointSet point_set_copy(point_set.points);
	{
		Polygon triangle(std::vector<Point> {low_hull.points[low_hull.size() - 2],
											 left_low,
											 top_hull.points[1]});
		Polygon left_low_hull = minAreaTriangularHullWithNodeRemoved(triangle, point_set_copy);
		min_area = std::min(total_area - triangle.area() + left_low_hull.area(), min_area);
	}

	// remove right_top point
	{
		Polygon triangle(std::vector<Point> {top_hull.points[top_hull.size() - 2],
											 right_top,
											 low_hull.points[1]});
		Polygon right_top_hull = minAreaTriangularHullWithNodeRemoved(triangle, point_set_copy);
		min_area = std::min(total_area - triangle.area() + right_top_hull.area(), min_area);
	}

	return min_area;
}

bool PointComparatorLeftLow::operator () (const Point& p1, const Point& p2) const {
	return (p1.x < p2.x || (p1.x == p2.x && p1.y < p2.y));
}

PointComparatorPolarAngleAndLength::PointComparatorPolarAngleAndLength(Point _origin)
	: origin(_origin) {}

bool PointComparatorPolarAngleAndLength::operator () (const Point& p1, const Point& p2) const {
	Vector v1(origin, p1);
	Vector v2(origin, p2);
	return (v2.isClockwiseRotation(v1) || (v1.isParallel(v2)
										   && (v1.lengthSquared() < v2.lengthSquared())));
}


int main() {
	std::ios_base::sync_with_stdio(false);
	for (;;) {
		int point_quantity;
		std::cin >> point_quantity;
		if (point_quantity == 0) {
			break;
		}
		PointSet point_set;
		point_set.points.reserve(point_quantity);
		for (int nail_index = 0; nail_index < point_quantity; ++nail_index) {
			int x, y;
			std::cin >> x >> y;
			point_set.points.push_back(Point(x, y));
		}

		double result_area = minAreaOfHullWithNodeRemoved(point_set);
		std::cout.precision(2);
		std::cout << std::fixed << result_area << '\n';
	}

	return 0;
}

