#pragma once

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>     // strtok, strcpy
#include <sys/types.h>  // ssize_t

#include "utilities.h"
#include "life_grid.h"

extern const size_t COMMAND_MAX_LENGTH;
extern const int    MASTER_ID;
extern const int    APPRENTICE_ID;
extern const char*  COMMAND_POSSIBLE_DELIMITERS;
extern const char*  ERROR_MESSAGE_NOT_STARTED;
extern const char*  ERROR_MESSAGE_NOT_STOPPED;
extern const char*  ERROR_MESSAGE_ALREADY_STARTED;
extern const char*  ERROR_MESSAGE_WRONG_COMMAND;
extern const char*  ERROR_MESSAGE_WRONG_TAG;
extern const char*  ERROR_MESSAGE_CANNOT_RECEIVE;
extern const char*  ERROR_MESSAGE_CANNOT_SEND;
extern const char*  FILENAME_RESULTS;

typedef enum {
    DUTY, GRID, START, 
    STATUS, RUN, ITER, 
    EXCHANGE, STOP, STOP_ALL, 
    TEST, FINISH, QUIT
} MessageTag;

typedef enum {WORKER_READY, WORKER_BUSY} WorkerState;

typedef struct {
    int worker_quantity;
    int height;
    int width;
    char* filename;
} CommandStartInfo;

typedef struct {
    int start_row;
    int end_row;
} WorkerDuty;

void normalize_command(char* command);
void distribute_duties(const Grid grid, const int worker_quantity, WorkerDuty** worker_duties);
ssize_t create_grid(CommandStartInfo info, Grid* grid);
ssize_t parse_start_command(char* command, CommandStartInfo* info);
ssize_t parse_run_command(char* command, int* iter_quantity);
void print_help();

void send_grid_layer(Grid grid, int start_row, int end_row, int from, int to, MessageTag tag);
void receive_grid_layer(Grid* grid, int start_row, int end_row, int from, MessageTag tag);

void broadcast_tag(int worker_quantity, int from, MessageTag tag);
