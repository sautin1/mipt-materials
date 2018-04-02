TEMPLATE = app
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt
CONFIG += link_pkgconfig
PKGCONFIG += opencv

SOURCES += \
    src/main.cpp \
    src/motion_estimator.cpp \
    src/os.cpp \
    src/timer.cpp

HEADERS += \
    src/motion_estimator.h \
    src/os.h \
    src/timer.h
