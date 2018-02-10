#ifndef CONVEX_HULL_H
#define CONVEX_HULL_H

#include <algorithm>
#include <vector>

#include "geometry.h"

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

#endif // CONVEX_HULL_H
