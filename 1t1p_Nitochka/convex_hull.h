#ifndef CONVEX_HULL_H
#define CONVEX_HULL_H

#include <algorithm>
#include <vector>

#include "geometry.h"

class ConvexHull {
public:
	ConvexHull(const PointSet& point_set, bool is_sorted, const Point& origin);
	Polygon getHullPolygon() const;
	std::vector<int> getHullNodesIndices() const;
	std::vector<int> getPointsIndicesAngleSorted() const;
	PointSet getAngleSortedPointSet() const;
	const PointSet& getPointSet() const;
private:
	const PointSet& point_set_;
	std::vector<int> points_sorted_;
	std::vector<int> hull_;
};

struct PointComparatorLeftLow {
	PointComparatorLeftLow(const PointSet& point_set);
	bool operator () (int i1, int i2) const;
private:
	const PointSet& pointSet_;
};

double min_area_shrunk_hull(const ConvexHull& hull);
double min_area_shrunk_point_set(const PointSet& point_set);

#endif // CONVEX_HULL_H
