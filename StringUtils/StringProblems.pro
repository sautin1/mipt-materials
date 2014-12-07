TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG -= qt
CONFIG += c++11

SOURCES += main.cpp \
    string_utils.cpp

HEADERS += \
    IvanShafranLib/find_all_occurrences.h \
    IvanShafranLib/suffix_tree.h \
    string_utils.h \
    test_string_utils.h

unix:!macx: LIBS += -lgtest
LIBS += -lpthread
