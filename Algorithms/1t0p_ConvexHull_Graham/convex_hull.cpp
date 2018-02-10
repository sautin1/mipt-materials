#include "convex_hull.h"

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
