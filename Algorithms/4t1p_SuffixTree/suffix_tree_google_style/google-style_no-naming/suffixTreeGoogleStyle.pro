TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG -= qt
CONFIG += c++11

SOURCES += main.cc \
    suffix_tree.cc \
    find_occurrences.cc

HEADERS += \
    suffix_tree.h \
    find_occurrences.h \
    test_suffix_tree.h \
    test_find_occurrences.h

unix:!macx: LIBS += -lgtest
LIBS += -lpthread
