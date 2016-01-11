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

extern const ssize_t EXITCODE_WRONG_WIDTH;
extern const ssize_t EXITCODE_WRONG_HEIGHT;

int is_alive_neighbor(const Grid grid, const int row, const int column);
CellState update_cell(const Grid grid, const int row, const int column);
void delete_grid(Grid grid);
ssize_t print_grid(FILE* stream, const Grid grid);
Grid generate_random_grid(const int height, const int width);
ssize_t empty_grid(const int height, const int width, Grid* grid);
CellState* parse_grid_line(char* line, const char delim, int* width);
ssize_t read_grid(const char* filename, Grid* grid);
void to_array(Grid gr, int** arr);
void from_array(Grid* grid, int* array, int size);
int* serialize_grid_layer(Grid grid, int start_row, int end_row, int* grid_layer_size);
void deserialize_grid_layer(Grid* grid, int start_row, int* layer_arr, int layer_arr_size);
