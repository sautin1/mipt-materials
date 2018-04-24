TEMPLATE = app
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt
CONFIG += link_pkgconfig
PKGCONFIG += opencv

SOURCES += main.cpp \
    qr_detector.cpp \
    timer.cpp

HEADERS += \
    qr_detector.h \
    timer.h
