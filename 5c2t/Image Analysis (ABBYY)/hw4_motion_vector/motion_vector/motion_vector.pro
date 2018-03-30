TEMPLATE = app
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt
CONFIG += link_pkgconfig
PKGCONFIG += opencv

SOURCES += main.cpp \
    motion_estimator.cpp \
    os.cpp

HEADERS += \
    motion_estimator.h \
    os.h
