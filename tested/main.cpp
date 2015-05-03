#include <iostream>
#include <fstream>
#include <algorithm>
#include <cmath>
#include <ctgmath>
#include <stdexcept>
#include <vector>

typedef double Coord;

const double kEpsilon = 1e-9;
const double kPi = 3.1415926535897932384626433;

bool equals(double x, double y);

struct Point {
    Coord x;
    Coord y;
    Point() = default;
    Point(Coord _x, Coord _y);
    bool operator == (const Point& other) const;
    bool operator < (const Point& other) const;
    Point operator + (const Point& other) const;
    Point operator * (const Coord mult) const;
};

struct PointSet {
    std::vector<Point> points;

    PointSet() = default;
    explicit PointSet(const std::vector<Point>& _points);
    explicit PointSet(size_t initial_size);
};

struct Vector {
    Coord x;
    Coord y;

    Vector() = default;
    Vector(Coord _x, Coord _y);
    Vector(const Point& from, const Point& to);
    Coord lengthSquared() const;
    double length() const;
    bool isClockwiseRotation(const Vector& other) const;
    bool isParallel(const Vector& other) const;
    bool isCodirect(const Vector& other) const;
    Vector getPerpendicular() const;
};

struct Segment: public Vector {
    Point st;
    Segment(const Point& from, const Point& to);
    Segment(const Point& p, const Vector& v);
};

struct Line {
    Point p0;
    Vector v;       // p = p0 + v
    Coord a, b, c;  // ax + by + c = 0
    Line(const Point& _p0, const Vector& _v);
    Line(Coord _a, Coord _b, Coord _c);
    Line getPerpendicular(const Point& p) const;
    Point getPoint(double t) const;
};

Coord crossProduct(const Vector& v1, const Vector& v2);
Coord dotProduct(const Vector& v1, const Vector& v2);

class Shape {
public:
    virtual double area() const = 0;
    virtual double perimeter() const = 0;
};

struct Polygon: public PointSet, public Shape {
    explicit Polygon(const std::vector<Point>& nodes);
    size_t size() const;
    double signedArea() const;
    double area() const;
    double perimeter() const;
protected:
    double area_;
    double perimeter_;
private:
    void countArea();
    void countPerimeter();
};

struct ConvexPolygon: public Polygon {
    explicit ConvexPolygon(const std::vector<Point>& nodes);
    void sortNodesCCW();
    void sortNodesCW();
};

class Circle: public Shape {
public:
    Point c;
    double rad;
    Circle(const Point& _c, double _rad);
    double area() const;
    double perimeter() const;
};

double distance(const Point& p, const Line& l);
Point protract(const Point& p, const Vector& v);

bool isOn(const Point& p, const Segment& seg);
bool isOn(const Point& point, const Polygon& poly);
bool isIn(const Point& point, const ConvexPolygon& poly);

std::vector<Point> removeFlatAngles(const std::vector<Point>& points);

ConvexPolygon minkowskiSum(ConvexPolygon p1, ConvexPolygon p2);

bool intersects(const Line& line, const Circle& circle);
bool intersects(ConvexPolygon p1, const ConvexPolygon& p2);
std::vector<Line> getTangents(Circle c1, Circle c2);

bool equals(double x, double y) {
    return std::abs(x - y) < kEpsilon;
}

Point::Point(Coord _x, Coord _y)
    : x(_x), y(_y) {}

bool Point::operator == (const Point& other) const {
    return equals(x, other.x) && equals(y, other.y);
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

Coord Vector::lengthSquared() const {
    return dotProduct(*this, *this);
}

double Vector::length() const {
    return sqrt(lengthSquared());
}

bool Vector::isClockwiseRotation(const Vector& other) const {
    return crossProduct(other, *this) < 0;
}

bool Vector::isParallel(const Vector& other) const {
    return equals(crossProduct(*this, other), 0);
}

bool Vector::isCodirect(const Vector& other) const {
    return dotProduct(*this, other) > 0;
}

Vector Vector::getPerpendicular() const {
    return Vector(y, -x);
}

Segment::Segment(const Point& from, const Point& to)
    : Vector(from, to), st(from) {}

Segment::Segment(const Point& p, const Vector& v)
    : Vector(v), st(p) {}

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
    if (equals(a, 0) && equals(b, 0)) {
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
    return std::abs(signedArea());
}

double Polygon::perimeter() const {
    return perimeter_;
}

void Polygon::countArea() {
    area_ = 0;
    if (size() == 1) {
        return;
    }
    Vector v1(points[0], points[1]);
    Vector v2;
    for (size_t node_index = 2; node_index < points.size(); ++node_index) {
        v2 = Vector(points[0], points[node_index]);
        area_ += crossProduct(v1, v2) / 2.0;
        v1 = v2;
    }
}

void Polygon::countPerimeter() {
    perimeter_ = 0;
    for (size_t node_index = 1; node_index < points.size(); ++node_index) {
        Vector v(points[node_index - 1], points[node_index]);
        perimeter_ += v.length();
    }
    perimeter_ += Vector(points.back(), points.front()).length();
}

ConvexPolygon::ConvexPolygon(const std::vector<Point>& nodes)
    : Polygon(nodes) {}

void ConvexPolygon::sortNodesCCW() {
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

void ConvexPolygon::sortNodesCW() {
    sortNodesCCW();
    std::reverse(points.begin(), points.end());
    area_ *= -1;
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
    return std::abs(l.a * p.x + l.b * p.y + l.c) / sqrt(pow(l.a, 2) + pow(l.b, 2));
}

Point protract(const Point& p, const Vector& v) {
    return Point(p.x + v.x, p.y + v.y);
}

bool isOn(const Point& p, const Segment& seg) {
    Segment seg_p(seg.st, p);
    return (p == seg.st) || (seg.isParallel(seg_p) && seg.isCodirect(seg_p)
        && seg_p.lengthSquared() <= seg.lengthSquared());
}

bool isOn(const Point& point, const Polygon& poly) {
    if (poly.points.size() == 1) {
        return (point == poly.points.front());
    }
    if (poly.points.size() == 2) {
        return isOn(point, Segment(poly.points.front(), poly.points.back()));
    }
    bool is_on = false;
    for (size_t i = 0; i < poly.size(); ++i) {
        int next = (i + 1) % poly.size();
        Segment seg(poly.points[i], poly.points[next]);
        is_on = isOn(point, seg);
        if (is_on) {
            break;
        }
    }
    return is_on;
}

// tested on mccme
bool isIn(const Point& point, const ConvexPolygon& poly) {
    if (poly.points.size() == 1) {
        return (point == poly.points.front());
    }
    if (poly.points.size() == 2) {
        return isOn(point, Segment(poly.points.front(), poly.points.back()));
    }
    Segment seg_cur (poly.points[0], poly.points[1]);
    Segment seg_next(poly.points[0], point);
    bool is_cw = seg_cur.isClockwiseRotation(seg_next);
    bool is_in = true;
    for (size_t i = 1; i < poly.size(); ++i) {
        int next = (i + 1) % poly.size();
        seg_cur  = Segment(poly.points[i], poly.points[next]);
        seg_next = Segment(poly.points[i], point);
        is_in = (is_cw == seg_cur.isClockwiseRotation(seg_next)) && !isOn(point, seg_cur);
        if (!is_in) {
            break;
        }
    }
    return is_in;
}

std::vector<Point> removeFlatAngles(const std::vector<Point>& points) {
    if (points.size() < 3) {
        return points;
    }
    std::vector<Point> new_points;
    for (size_t check = 0; check < points.size(); ++check) {
        int prev;
        prev = check - 1;
        if (prev < 0) {
            prev += points.size();
        }
        size_t next = (check + 1) % points.size();
        Segment seg(points[prev], points[next]);
        if (!isOn(points[check], seg)) {
            new_points.push_back(points[check]);
        }
    }
    return new_points;
}

ConvexPolygon minkowskiSum(ConvexPolygon p1, ConvexPolygon p2) {
    p1.sortNodesCCW();
    p2.sortNodesCCW();
    std::vector<Point> sum;
    sum.reserve(p1.size() + p2.size());
    int left_point1 = std::min_element(p1.points.begin(), p1.points.end()) - p1.points.begin();
    int left_point2 = std::min_element(p2.points.begin(), p2.points.end()) - p2.points.begin();
    sum.push_back(p1.points[left_point1] + p2.points[left_point2]);
    int p1_cur = left_point1;
    int p2_cur = left_point2;
    bool is_traversed1 = false;
    bool is_traversed2 = false;
    do {
        int p1_next = (p1_cur + 1) % p1.points.size();
        int p2_next = (p2_cur + 1) % p2.points.size();
        Vector v1(p1.points[p1_cur], p1.points[p1_next]);
        Vector v2(p2.points[p2_cur], p2.points[p2_next]);
        if (is_traversed2 || (!is_traversed1 && v1.isClockwiseRotation(v2))) {
            p1_cur = p1_next;
            sum.push_back(protract(sum.back(), v1));
            is_traversed1 = (p1_cur == left_point1);
        } else {
            p2_cur = p2_next;
            sum.push_back(protract(sum.back(), v2));
            is_traversed2 = (p2_cur == left_point2);
        }
    } while (!is_traversed1 || !is_traversed2);
    sum.pop_back();
    return ConvexPolygon(removeFlatAngles(sum));
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
    Point origin(0, 0);
    return isIn(origin, sum) || isOn(origin, sum);
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
            discr = sqrt(std::abs(discr));
            double a = (rad * c2.c.x - c2.c.y * discr) / len;
            double b = (rad * c2.c.y + c2.c.x * discr) / len;
            tangents.push_back(Line(a, b, c - a * c1.c.x - b * c1.c.y));
        }
    }
    return tangents;
}

ConvexPolygon readConvexPolygon(std::istream& fin) {
    size_t poly_size;
    fin >> poly_size;
    std::vector<Point> poly_points;
    poly_points.reserve(poly_size);
    for (size_t i = 0; i < poly_size; ++i) {
        Coord x, y;
        fin >> x >> y;
        poly_points.push_back(Point(x, y));
    }
    return ConvexPolygon(poly_points);
}

void printConvexPolygon(const ConvexPolygon& poly) {
    for (size_t i = 0; i < poly.points.size(); ++i) {
        std::cout << poly.points[i].x << " ; " << poly.points[i].y << "\n";
    }
}

int main() {
    ConvexPolygon p1 = readConvexPolygon(std::cin);
    ConvexPolygon p2 = readConvexPolygon(std::cin);

    if (intersects(p1, p2)) {
        std::cout << "YES\n";
    } else {
        std::cout << "NO\n";
    }
}
