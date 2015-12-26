#include "commands.h"

const char* COMMAND_POSSIBLE_DELIMITERS     = " ";
const char* ERROR_MESSAGE_NOT_STARTED       = "Wrong command: not started yet";
const char* ERROR_MESSAGE_NOT_STOPPED       = "Wrong command: not stopped yet";
const char* ERROR_MESSAGE_ALREADY_STARTED   = "Wrong command: already started";
const char* FILENAME_RESULTS                = "result.txt";

void normalize_command(char* command) {
    command = trim(command);
    command = tolower_str(command);
}

void distribute_duties(const Grid grid, const int worker_quantity, const int iter_quantity, 
                       WorkerDuty** worker_duties) {
    *worker_duties = (WorkerDuty*)malloc(worker_quantity * sizeof(WorkerDuty));
    int worker_part = grid.height / worker_quantity;
    worker_part += (grid.height % worker_quantity == 0) ? 0 : 1;
    for (int i = 0; i < worker_quantity; ++i) {
        (*worker_duties)[i].iter_quantity = iter_quantity;
        (*worker_duties)[i].start_row = (i == 0) ? 0 : (*worker_duties)[i-1].end_row;
        (*worker_duties)[i].end_row = (*worker_duties)[i].start_row + worker_part;
    }
    (*worker_duties)[worker_quantity-1].end_row = grid.height;
}

ssize_t create_grid(CommandStartInfo info, Grid* grid) {
    if (info.filename) {
        int error = read_grid(info.filename, grid);
        if (error) {
            return -1;
        }
    } else {
        if (info.height <= 0) {
            info.height = rand() % (GRID_MAX_DIMENSION - 1) + GRID_MIN_DIMENSION;
            info.width  = rand() % (GRID_MAX_DIMENSION - 1) + GRID_MIN_DIMENSION;
        }
        *grid = generate_random_grid(info.height, info.width);
    }
    return 0;
}

ssize_t parse_start_command(char* command, CommandStartInfo* info) {
    char* token = strtok(command, COMMAND_POSSIBLE_DELIMITERS);
    if (!token) {
        fprintf(stderr, "Wrong start command format\n");
        return -1;
    }
    
    token = strtok(NULL, COMMAND_POSSIBLE_DELIMITERS);
    if (!token || (info->worker_quantity = strtol(token, NULL, 10)) <= 0) {
        fprintf(stderr, "Wrong worker quantity\n");
        return -1;
    }

    token = strtok(NULL, COMMAND_POSSIBLE_DELIMITERS);
    if (!token) {
        // completely random grid
        info->height = -1;
        info->width  = -1;
        info->filename = NULL;
        return 0;
    }
    char* last_token = strtok(NULL, COMMAND_POSSIBLE_DELIMITERS);
    if (!last_token) {
        // from file
        info->filename = token;
        info->height = -1;
        info->width = -1;
    } else {
        // dimensions given
        info->filename = NULL;
        info->height = strtol(token, NULL, 10);
        info->width  = strtol(last_token, NULL, 10);
        if (info->height <= 0 || info->width <= 0) {
            fprintf(stderr, "Wrong grid dimensions\n");
            return -1;
        }
    }
    return 0;
}

ssize_t parse_run_command(char* command, int* iter_quantity) {
    char* token = strtok(command, COMMAND_POSSIBLE_DELIMITERS);
    if (!token) {
        fprintf(stderr, "Wrong run command format\n");
        return -1;
    }
    
    token = strtok(NULL, COMMAND_POSSIBLE_DELIMITERS);
    if (!token || (*iter_quantity = strtol(token, NULL, 10)) <= 0) {
        fprintf(stderr, "Wrong iteration quantity\n");
        return -1;
    }
    return 0;
}

void print_help() {
    printf("Available commands:\n");
    printf("\tstart <worker_quantity>\n");
    printf("\tstart <worker_quantity> <filename>\n");
    printf("\tstart <worker_quantity> <height> <width>\n");
    printf("\tstatus\n");
    printf("\trun <iteration_quantity>\n");
    printf("\tstop\n");
    printf("\tquit\n");
    printf("\thelp\n");
}
