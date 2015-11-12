#pragma once

#include <ctype.h>      // isspace, tolower
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>  // ssize_t

ssize_t count_char_occur(const char* haystack, const char needle);
char* read_file(const char* filename);
ssize_t count_file_lines(const char* filename);
char* trim(char* line);
char* tolower_str(char* line);

ssize_t count_char_occur(const char* haystack, const char needle) {
    if (!haystack) {
        return -1;
    }
    size_t result = 0;
    char* occurence = strchr(haystack, needle);
    while (occurence != NULL) {
        ++result;
        occurence = strchr(occurence + 1, needle);
    }
    return result;
}

char* read_file(const char* filename) {
    FILE* stream = fopen(filename, "rb");
    if (!stream) {
        perror("read_file");
        return NULL;
    }
    fseek(stream, 0, SEEK_END);
    long file_size = ftell(stream);
    fseek(stream, 0, SEEK_SET);

    char* contents = (char*)malloc(file_size + 1);
    fread(contents, file_size, 1, stream);
    fclose(stream);

    contents[file_size] = 0;
    return contents;
}

ssize_t count_file_lines(const char* filename) {
    char* contents = read_file(filename);
    if (!contents) {
        return -1;
    }
    int result = count_char_occur(contents, '\n');
    size_t file_length = strlen(contents);
    if (contents[file_length - 1] != '\n') {
        ++result;
    }
    if (contents) {
        free(contents);
    }
    return result;
}

char* trim(char* line) {
    size_t length = strlen(line);
    int line_start = 0;
    while (isspace(line[line_start])) {
        ++line_start;
    }
    if (line_start > 0) {
        memmove(line, line + line_start, length);
        length -= line_start;
    }
    for (int i = length - 1; i >= 0 && isspace(line[i]); --i) {
        line[i] = 0;
    }
    return line;
}

char* tolower_str(char* line) {
    for (int i = 0; line[i] != 0; ++i) {
        line[i] = tolower(line[i]);
    }
    return line;
}