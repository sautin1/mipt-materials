#pragma once

#include <ctype.h>      // isspace, tolower
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>  // ssize_t

extern const ssize_t EXITCODE_FILE_NOT_FOUND;
extern const ssize_t EXITCODE_NULL_ARGUMENT;

ssize_t count_char_occur(const char* haystack, const char needle);
char* read_file(const char* filename);
ssize_t count_file_lines(const char* filename);
char* trim(char* line);
char* tolower_str(char* line);
