#pragma once

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>     // strtok
#include <sys/types.h>  // ssize_t

#include "utilities.h"
#include "life_grid.h"

extern const char* COMMAND_POSSIBLE_DELIMITERS;
extern const char* ERROR_MESSAGE_NOT_STARTED;
extern const char* ERROR_MESSAGE_NOT_STOPPED;
extern const char* ERROR_MESSAGE_ALREADY_STARTED;
extern const char* FILENAME_RESULTS;

typedef struct {
    int worker_quantity;
    int height;
    int width;
    char* filename;
} CommandStartInfo;

typedef struct {
    int iter_quantity;
    int start_row;
    int end_row;
} WorkerDuty;

typedef struct {
    pthread_t** workers;
    WorkerDuty** duties;
} ManagerInfo;

void normalize_command(char* command);
void distribute_duties(const Grid grid, const int worker_quantity, const int iter_quantity, 
                       WorkerDuty** worker_duties);
ssize_t create_grid(CommandStartInfo info, Grid* grid);
ssize_t parse_start_command(char* command, CommandStartInfo* info);
ssize_t parse_run_command(char* command, int* iter_quantity);
void print_help();