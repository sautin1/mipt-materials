#ifndef CONVEX_HULL_H
#define CONVEX_HULL_H

#include <algorithm>
#include <functional>
#include <stdexcept>
#include <vector>

#include "geometry.h"

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

#endif // CONVEX_HULL_H
