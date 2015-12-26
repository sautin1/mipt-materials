#include "life_grid.h"

const int GRID_MAX_DIMENSION = 10;
const int GRID_MIN_DIMENSION = 2;
const double CELL_ALIVE_PROBABILITY = 0.6;
const char CELL_ALIVE_CHAR = '+';
const char CELL_DEAD_CHAR  = '.';
const char CELL_DELIMITER  = ',';
const int LINE_MAX_LENGTH = 256;

const char* ERROR_MESSAGE_WRONG_HEIGHT = "Wrong grid height";
const char* ERROR_MESSAGE_WRONG_WIDTH  = "Wrong grid width";

int is_alive_neighbor(const Grid grid, const int row, const int column) {
    if (row < 0 || column < 0 || row >= grid.height || column >= grid.width) {
        return 0;
    }
    return (grid.states[row][column] == CellAlive) ? 1 : 0;
}

CellState update_cell(const Grid grid, const int row, const int column) {
    int neighbors_quantity = 0;

    for (int delta_row = -1; delta_row <= 1; ++delta_row) {
        for (int delta_column = -1; delta_column <= 1; ++delta_column) {
            if (delta_row == 0 && delta_column == 0) {
                continue;
            }
            neighbors_quantity += is_alive_neighbor(grid, row + delta_row, column + delta_column);
        }
    }
    
    CellState result;
    if (neighbors_quantity < 2 || neighbors_quantity > 3) {
        result = CellDead;
    } else if (grid.states[row][column] == CellAlive) {
        result = CellAlive;
    } else {
        result = (neighbors_quantity == 3) ? CellAlive : CellDead;
    }

    return result;
}

void delete_grid(Grid grid) {
    for (int i = 0; i < grid.height; ++i) {
        free(grid.states[i]);
        grid.states[i] = NULL;
    }
    free(grid.states);
    grid.states = NULL;
}

ssize_t print_grid(FILE* stream, const Grid grid) {
    if (!stream) {
        perror("print_grid");
        return -1;
    }
    for (int i = 0; i < grid.height; ++i) {
        for (int j = 0; j < grid.width; ++j) {
            fprintf(stream, "%c ", (grid.states[i][j] == CellDead) ? CELL_DEAD_CHAR: 
                                                                     CELL_ALIVE_CHAR);
        }
        fprintf(stream, "\n");
    }
    return 0;
}

Grid generate_random_grid(const int height, const int width) {
    Grid grid;
    grid.height = height;
    grid.width = width;
    grid.states = (CellState**)malloc(height * sizeof(CellState*));
    for (int i = 0; i < height; ++i) {
        grid.states[i] = (CellState*)malloc(width * sizeof(CellState));
        for (int j = 0; j < width; ++j) {
            double probability = 1.0 * rand() / RAND_MAX;
            if (probability <= CELL_ALIVE_PROBABILITY) {
                grid.states[i][j] = CellAlive;
            } else {
                grid.states[i][j] = CellDead;
            }
        }
    }
    return grid;
}


ssize_t empty_grid(const int height, const int width, Grid* grid) {
    if (height <= 0 || width <= 0) {
        fprintf(stderr, "empty_grid: %s\n", (height <= 0)? ERROR_MESSAGE_WRONG_HEIGHT : 
                                                           ERROR_MESSAGE_WRONG_WIDTH);
        return -1;
    }
    grid->height = height;
    grid->width  = width;
    grid->states = (CellState**)calloc(height, sizeof(CellState*));
    for (int i = 0; i < height; ++i) {
        grid->states[i] = (CellState*)malloc(width * sizeof(CellState));
    }
    return 0;
}

CellState* parse_grid_line(char* line, const char delim, int* width) {
    *width = count_char_occur(line, delim) + 1;
    CellState* grid_line = (CellState*)malloc(*width * sizeof(CellState));
    int state_index = 0;
    int flag = 1;
    for (int i = 0; line[i] != '\0'; ++i) {
        if (flag && line[i] == CELL_DEAD_CHAR) {
            grid_line[state_index++] = CellDead;
            flag = 0;
        } else if (flag && line[i] == CELL_ALIVE_CHAR) {
            grid_line[state_index++] = CellAlive;
            flag = 0;
        } else if (!flag && line[i] == CELL_DELIMITER){
            flag = 1;
        } // else it's rubbish - just ignore it
    }

    return grid_line;
}

int read_grid(const char* filename, Grid* grid) {
    grid->height = count_file_lines(filename);
    if (grid->height <= 0) {
        fprintf(stderr, "read_grid: %s\n", ERROR_MESSAGE_WRONG_HEIGHT);
        return -1;
    }
    grid->states = (CellState**)calloc(grid->height, sizeof(CellState*));
    
    FILE* stream = fopen(filename, "r");
    if (!stream) {
        perror("read_grid");
        return -1;
    }

    char* line = (char*)malloc(LINE_MAX_LENGTH * sizeof(char));
    int exit_code = 0;
    for (int i = 0; i < grid->height; ++i) {
        fgets(line, LINE_MAX_LENGTH, stream);
        int width;
        grid->states[i] = parse_grid_line(line, CELL_DELIMITER, &width);

        if (i == 0) {
            grid->width = width;
        } else if (grid->width != width) {
            fprintf(stderr, "read_grid: %s\n", ERROR_MESSAGE_WRONG_WIDTH);
            exit_code = -1;
            break;
        }
    }
    fclose(stream);
    if (line) {
        free(line);
    }
    return exit_code;
}
