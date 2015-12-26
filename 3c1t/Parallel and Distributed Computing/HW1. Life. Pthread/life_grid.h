#pragma once
#include <stdio.h>
#include "utilities.h"

typedef enum {CellDead, CellAlive} CellState;

typedef struct {
    int height;
    int width;
    CellState** states;
} Grid;

extern const int GRID_MAX_DIMENSION;
extern const int GRID_MIN_DIMENSION;
extern const double CELL_ALIVE_PROBABILITY;
extern const char CELL_ALIVE_CHAR;
extern const char CELL_DEAD_CHAR;
extern const char CELL_DELIMITER;
extern const int LINE_MAX_LENGTH;

extern const char* ERROR_MESSAGE_WRONG_HEIGHT;
extern const char* ERROR_MESSAGE_WRONG_WIDTH;

int is_alive_neighbor(const Grid grid, const int row, const int column);
CellState update_cell(const Grid grid, const int row, const int column);
void delete_grid(Grid grid);
ssize_t print_grid(FILE* stream, const Grid grid);
Grid generate_random_grid(const int height, const int width);
ssize_t empty_grid(const int height, const int width, Grid* grid);
CellState* parse_grid_line(char* line, const char delim, int* width);
int read_grid(const char* filename, Grid* grid);