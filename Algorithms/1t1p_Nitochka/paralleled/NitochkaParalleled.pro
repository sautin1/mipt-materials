TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG -= qt
CONFIG += c++11

SOURCES += main.cpp \
    convex_hull.cpp \
    geometry.cpp \
    tester.cpp

HEADERS += \
    convex_hull.h \
    geometry.h \
    tester.h

QMAKE_CXXFLAGS += -fopenmp
QMAKE_LFLAGS += -fopenmp
LIBS += -lgomp -lpthread
