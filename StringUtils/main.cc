// Copyright 2014, MIPT
// All rights reserved.
//
// Author: sautin1@yandex.ru (Andrew Sautin)

#include <iostream>

#include "string_utils.h"
#include "test_string_utils.h"

int test(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

int main(int argc, char** argv) {
  int test_result = test(argc, argv);
  return test_result;
}

