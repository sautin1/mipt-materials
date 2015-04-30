#include "geometry.h"

bool equals(double x, double y) {
    return abs(x - y) < kEpsilon;
}

Point::Point(Coord _x, Coord _y)
    : x(_x), y(_y) {}

bool Point::operator == (const Point& other) const {
    return x == other.x && y == other.y;
}

bool Point::operator < (const Point& other) const {
    return (x < other.x || (x == other.x && y < other.y));
}

Point Point::operator + (const Point& other) const {
    return Point(x + other.x, y + other.y);
}

Point Point::operator * (const Coord mult) const {
    return Point(x * mult, y * mult);
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
    return sqrt(lengthSquared());
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
    : PointSet(nodes) {
    countArea();
    countPerimeter();
}

size_t Polygon::size() const {
    return points.size();
}

double Polygon::signedArea() const {
    return area_;
}

double Polygon::area() const {
    return abs(signedArea());
}

double Polygon::perimeter() const {
    return perimeter_;
}

void Polygon::sortNodesCCW() {
    if (points.size() < 3) {
        return;
    }
    Vector v1(points[0], points[1]);
    Vector v2(points[1], points[2]);
    if (v2.isClockwiseRotation(v1)) {
        std::reverse(points.begin(), points.end());
        area_ *= -1;
    }
}

void Polygon::sortNodesCW() {
    sortNodesCCW();
    std::reverse(points.begin(), points.end());
    area_ *= -1;
}

double Polygon::countArea() {
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

double Polygon::countPerimeter() {
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

double Circle::area() const {
    return kPi * pow(rad, 2);
}

double Circle::perimeter() const {
    return 2 * kPi * rad;
}

double distance(const Point& p, const Line& l) {
    return abs(l.a * p.x + l.b * p.y + l.c) / sqrt(pow(l.a, 2) + pow(l.b, 2));
}

Point protract(const Point& p, const Vector& v) {
    return Point(p.x + v.x, p.y + v.y);
}

ConvexPolygon::ConvexPolygon(const std::vector<Point>& nodes)
    : Polygon(nodes) {}

template <class InputIterator>
ConvexPolygon convexHull(const InputIterator& begin_it, const InputIterator& end_it) {
    std::vector<Point> hull;
    if (end_it - begin_it == 0) {
        return ConvexPolygon(hull);
    }
    hull.push_back(*begin_it);
    if (end_it - begin_it == 1) {
        return ConvexPolygon(hull);
    }
    hull.push_back(*(begin_it + 1));
    if (end_it - begin_it == 2) {
        return ConvexPolygon(hull);
    }
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
    if (hull.size() < 3) {
        // wrong direction was chosen
        std::vector<Point> newPoints(begin_it, end_it);
        std::reverse(newPoints.begin(), newPoints.end());
        hull = convexHull(newPoints.begin(), newPoints.end()).points;
        std::reverse(hull.begin(), hull.end());
    }
    return ConvexPolygon(hull);
}

ConvexPolygon minkowskiSum(ConvexPolygon p1, ConvexPolygon p2) {
    p1.sortNodesCCW();
    p2.sortNodesCCW();
    std::vector<Point> sum;
    int left_point1 = std::min_element(p1.points.begin(), p1.points.end()) - p1.points.begin();
    int left_point2 = std::min_element(p2.points.begin(), p2.points.end()) - p2.points.begin();
    sum.push_back(p1.points[left_point1] + p2.points[left_point2]);
    int p1_cur = left_point1;
    int p2_cur = left_point2;
    bool is_traversed1 = false;
    do {
        int p1_next = (p1_cur + 1) % p1.points.size();
        int p2_next = (p2_cur + 1) % p2.points.size();
        Vector v1(p1.points[p1_cur], p1.points[p1_next]);
        Vector v2(p2.points[p2_cur], p2.points[p2_next]);
        if (!is_traversed1 && v1.isClockwiseRotation(v2)) {
            p1_cur = p1_next;
            sum.push_back(protract(sum.back(), v1));
            is_traversed1 = (p1_cur == left_point1);
        } else {
            p2_cur = p2_next;
            sum.push_back(protract(sum.back(), v2));
        }
    } while (p1_cur != left_point1 || p2_cur != left_point2);
    sum.pop_back();
    return convexHull(sum.begin(), sum.end());
}

bool isIn(const Point& p, const Vector& v) {
    // need to be implemented
    return false;
}

bool isIn(const Point& point, const Polygon& poly) {
    if (poly.points.size() == 1) {
        return (point == poly.points.front());
    }
    if (poly.points.size() == 2) {
        return isIn(point, Vector(poly.points.front(), poly.points.back()));
    }
    Vector v_cur (poly.points[0], poly.points[1]);
    Vector v_next(poly.points[0], point);
    bool is_cw = v_cur.isClockwiseRotation(v_next);
    bool is_in = true;
    for (int i = 1; i != 0; i = (i + 1) % poly.size()) {
        int next = (i + 1) % poly.size();
        v_cur  = Vector(poly.points[i], poly.points[next]);
        v_next = Vector(poly.points[i], point);
        is_in = (is_cw == v_cur.isClockwiseRotation(v_next));
        if (!is_in) {
            break;
        }
    }
    return is_in;
}

bool intersects(const Line& line, const Circle& circle) {
    double dist = distance(circle.c, line);
    return equals(dist, circle.rad) || dist < circle.rad;
}

bool intersects(ConvexPolygon p1, const ConvexPolygon& p2) {
    for (size_t i = 0; i < p1.points.size(); ++i) {
        p1.points[i] = p1.points[i] * (-1);
    }
    ConvexPolygon sum = minkowskiSum(p1, p2);
    return isIn(Point(0, 0), sum);
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
            if (discr < -kEpsilon) {
                throw std::logic_error("Got negative discriminant");
            }
            discr = sqrt(abs(discr));
            double a = (rad * c2.c.x - c2.c.y * discr) / len;
            double b = (rad * c2.c.y + c2.c.x * discr) / len;
            tangents.push_back(Line(a, b, c - a * c1.c.x - b * c1.c.y));
        }
    }
    return tangents;
}
