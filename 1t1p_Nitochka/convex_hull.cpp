#include "convex_hull.h"

ConvexHull::ConvexHull(const PointSet& point_set, bool is_sorted, const Point& origin)
	: point_set_(point_set) {
	if (point_set.points.size() < 3) {
		throw std::logic_error("Too few nodes to build a convex hull");
	}
	// fill permutation
	points_sorted_.reserve(point_set_.points.size());
	for (size_t point_index = 0; point_index < point_set_.points.size(); ++point_index) {
		points_sorted_.push_back(point_index);
	}
	// sort points by polar angle
	if (!is_sorted) {
		std::sort(points_sorted_.begin(), points_sorted_.end(),
				  PointsIndexPolarAngleAndLengthComparator(point_set_, origin));
	}

	// put first 2 points
	hull_.push_back(points_sorted_[0]);
	hull_.push_back(points_sorted_[1]);
	// build hull
	Vector v_old(point_set_[hull_[0]], point_set_[hull_[1]]);
	for (size_t point_index = 2; point_index < points_sorted_.size(); ++point_index) {
		Vector v_new(point_set_[hull_.back()], point_set_[points_sorted_[point_index]]);
		while (hull_.size() > 1 && !v_new.isClockwiseRotation(v_old)) {
			hull_.pop_back();
			v_new = Vector(point_set_[hull_.back()], point_set_[points_sorted_[point_index]]);
			v_old = Vector(point_set_[hull_[hull_.size() - 2]], point_set_[hull_.back()]);
		}
		hull_.push_back(points_sorted_[point_index]);
		v_old = v_new;
	}
}

Polygon ConvexHull::getHullPolygon() const {
	std::vector<Point> nodes;
	nodes.reserve(hull_.size());
	for (size_t node_index = 0; node_index < hull_.size(); ++node_index) {
		nodes.push_back(point_set_[hull_[node_index]]);
	}
	return Polygon(nodes);
}

std::vector<int> ConvexHull::getHullNodesIndices() const {
	return hull_;
}

std::vector<int> ConvexHull::getPointsIndicesAngleSorted() const {
	return points_sorted_;
}

PointSet ConvexHull::getAngleSortedPointSet() const {
	PointSet pointSet;
	pointSet.points.reserve(points_sorted_.size());
	for (size_t point_index = 0; point_index < points_sorted_.size(); ++point_index) {
		pointSet.points.push_back(point_set_[points_sorted_[point_index]]);
	}
	return pointSet;
}

const PointSet& ConvexHull::getPointSet() const {
	return point_set_;
}

PointComparatorLeftLow::PointComparatorLeftLow(const PointSet& point_set)
	: pointSet_(point_set) {}

bool PointComparatorLeftLow::operator () (int i1, int i2) const {
	return (pointSet_[i1].x < pointSet_[i2].x ||
			(pointSet_[i1].x == pointSet_[i2].x && pointSet_[i1].y < pointSet_[i2].y));
}

double min_area_shrunk_hull(const ConvexHull& hull) {
	Polygon hull_poly = hull.getHullPolygon();
	std::vector<int> hull_indices = hull.getHullNodesIndices();
	std::vector<int> points_sorted = hull.getPointsIndicesAngleSorted();
	const PointSet& point_set = hull.getPointSet();
	double min_area = hull_poly.area();

	int sorted_point_index = 0;
	for (size_t hull_node_index = 1; hull_node_index < hull_poly.size() - 1; ++hull_node_index) {
		int prev_node = hull_indices[hull_node_index - 1];
		int remove_node = hull_indices[hull_node_index];
		int next_node = hull_indices[hull_node_index + 1];
		Polygon triangle(std::vector<Point> {point_set[prev_node],
											 point_set[remove_node],
											 point_set[next_node]});
		double area = hull_poly.area() - triangle.area();
		PointSet triangle_inner_points(std::vector<Point> (1, point_set[prev_node]));
		Vector triangle_base(point_set[prev_node], point_set[next_node]);
		int remove_hull_point_index;
		while (points_sorted[sorted_point_index] != next_node) {
			if (points_sorted[sorted_point_index] == remove_node) {
				remove_hull_point_index = sorted_point_index;
			} else {
				Vector v_point(point_set[prev_node], point_set[points_sorted[sorted_point_index]]);
				if (triangle_base.isClockwiseRotation(v_point)) {
					triangle_inner_points.points.push_back(point_set[points_sorted[sorted_point_index]]);
				}
			}
			++sorted_point_index;
		}
		triangle_inner_points.points.push_back(point_set[next_node]);
		if (triangle_inner_points.points.size() >= 3) {
			// build convex hull on them and add its area
			ConvexHull inner_up_hull(triangle_inner_points, true, hull_poly.nodes.front());
			area += inner_up_hull.getHullPolygon().area();
		}
		min_area = std::min(min_area, area);
		sorted_point_index = remove_hull_point_index;
	}
	return min_area;
}

double min_area_shrunk_point_set(const PointSet& point_set) {
	std::vector<int> indices;
	indices.reserve(point_set.points.size());
	for (size_t point_index = 0; point_index < point_set.points.size(); ++point_index) {
		indices.push_back(point_index);
	}
	PointComparatorLeftLow comparator(point_set);
	int left_low_point = *(std::min_element(indices.begin(), indices.end(), comparator));
	int right_high_point = *(std::max_element(indices.begin(), indices.end(), comparator));
	PointSet up_point_set;
	PointSet down_point_set;
	Vector diameter(point_set[left_low_point], point_set[right_high_point]);
	for (size_t point_index = 0; point_index < point_set.points.size(); ++point_index) {
		Vector v(point_set[left_low_point], point_set[point_index]);
		if (diameter.isClockwiseRotation(v) || diameter.isParallel(v)) {
			up_point_set.points.push_back(point_set[point_index]);
		}
		if (!diameter.isClockwiseRotation(v)) {
			down_point_set.points.push_back(point_set[point_index]);
		}
	}
	ConvexHull up_hull(up_point_set, false, point_set[left_low_point]);
	ConvexHull down_hull(down_point_set, false, point_set[right_high_point]);
	Polygon up_hull_poly = up_hull.getHullPolygon();
	Polygon down_hull_poly = down_hull.getHullPolygon();
	double area = up_hull_poly.area() + down_hull_poly.area();
	double min_area = std::min(up_hull_poly.area() + min_area_shrunk_hull(down_hull),
							   down_hull_poly.area() + min_area_shrunk_hull(up_hull));

	/*int sorted_point_index = 0;
	for (size_t hull_node_index = 1; hull_node_index < hull_poly.size() - 1; ++hull_node_index) {
		int prev_node = hull_indices[hull_node_index - 1];
		int remove_node = hull_indices[hull_node_index];
		int next_node = hull_indices[hull_node_index + 1];
		Polygon triangle(std::vector<Point> {point_set[prev_node],
											 point_set[remove_node],
											 point_set[next_node]});
		double area = hull_poly.area() - triangle.area();
		PointSet triangle_inner_points(std::vector<Point> (1, point_set[prev_node]));
		Vector triangle_base(point_set[prev_node], point_set[next_node]);
		int remove_hull_point_index;
		while (points_sorted[sorted_point_index] != next_node) {
			if (points_sorted[sorted_point_index] == remove_node) {
				remove_hull_point_index = sorted_point_index;
			} else {
				Vector v_point(point_set[prev_node], point_set[points_sorted[sorted_point_index]]);
				if (triangle_base.isClockwiseRotation(v_point)) {
					triangle_inner_points.points.push_back(point_set[points_sorted[sorted_point_index]]);
				}
			}
			++sorted_point_index;
		}
		triangle_inner_points.points.push_back(point_set[next_node]);
		if (triangle_inner_points.points.size() >= 3) {
			// build convex hull on them and add its area
			ConvexHull inner_up_hull(triangle_inner_points, true, hull_poly.nodes.front());
			area += inner_up_hull.getHullPolygon().area();
		}
		min_area = std::min(min_area, area);
		sorted_point_index = remove_hull_point_index;
	}*/

	return min_area;
}
