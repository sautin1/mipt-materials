#include <fstream>
#include <iomanip>
#include <ios>
#include <iostream>
#include <string>
#include <vector>

#include "tsp.h"

const std::string kFilePath          = "../tests/";
const std::string kFileInputSuffix   = ".in\0";
const std::string kFileOutputSuffix  = ".out";
const std::string kAnsiiDefault      = "\033[0m";
const std::string kAnsiiRedBold      = "\033[1;31m";
const std::string kAnsiiGreenBold    = "\033[1;32m";
const int         kColumnWidth       = 20;

struct Test {
    std::string name;
    bool is_planar;
    Test(const std::string& _name, bool _is_planar)
        : name(_name), is_planar(_is_planar) {}
};

const std::vector<Test> kTestSet {
    Test("nonplanar1", false),
    Test("nonplanar2", false),
    Test("pentagon"  , true),
    Test("djibouti"  , true),
    Test("luxembourg", true),
    Test("oman"      , true),
    Test("nicaragua" , true),
    Test("qatar"     , true),
    Test("rwanda"    , true),
    Test("zimbabwe"  , true),
    Test("canada"    , true),
    Test("uruguay"   , true),
    Test("sahara"    , true),
};

struct Point {
    CostT x, y;
    Point(CostT _x, CostT _y) : x(_x), y(_y) {}
};

CostT euclideanDistance(const Point& a, const Point& b);

CompleteGraph readNonPlanarCompleteGraph(const std::string& filename);
CompleteGraph readPlanarCompleteGraph(const std::string& filename);
CostT readResult(const std::string& filename);

std::string generateFilename(const std::string& testname, bool is_input);

bool test(const Test& test_case);
void testAll();
