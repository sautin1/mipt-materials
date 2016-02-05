#include "commands.h"

const size_t COMMAND_MAX_LENGTH             = 255;
const int    MASTER_ID                      = 0;
const int    APPRENTICE_ID                  = 1;
const char*  COMMAND_POSSIBLE_DELIMITERS    = " ";
const char*  ERROR_MESSAGE_NOT_STARTED      = "Wrong command: not started yet";
const char*  ERROR_MESSAGE_NOT_STOPPED      = "Wrong command: not stopped yet";
const char*  ERROR_MESSAGE_ALREADY_STARTED  = "Wrong command: already started";
const char*  ERROR_MESSAGE_WRONG_COMMAND    = "Wrong command\nType \'help\' to get the list of possible commands";
const char*  ERROR_MESSAGE_WRONG_TAG        = "Wrong message tag";
const char*  ERROR_MESSAGE_CANNOT_RECEIVE   = "Cannot receive the message";
const char*  ERROR_MESSAGE_CANNOT_SEND      = "Cannot send the message";
const char*  ERROR_MESSAGE_SMALL_GRID       = "Too small grid height for this number of processes";
const char*  FILENAME_RESULTS               = "result.txt";
const char*  PATH_GRID_FOLDER               = "../grids/";

void normalize_command(char* command) {
    command = trim(command);
    command = tolower_str(command);
}

void distribute_duties(const Grid grid, const int worker_quantity, WorkerDuty** worker_duties) {
    *worker_duties = (WorkerDuty*)malloc(worker_quantity * sizeof(WorkerDuty));
    int worker_part = grid.height / worker_quantity;
    int remain = grid.height % worker_quantity;
    for (int i = 0; i < worker_quantity; ++i) {
        (*worker_duties)[i].start_row = (i == 0) ? 0 : (*worker_duties)[i-1].end_row;
        (*worker_duties)[i].end_row = (*worker_duties)[i].start_row + worker_part;
        if (remain > 0) {
            ++(*worker_duties)[i].end_row;
            --remain;
        }
    }
    (*worker_duties)[worker_quantity-1].end_row = grid.height;
}

ssize_t create_grid(CommandStartInfo info, Grid* grid) {
    if (info.filename) {
        int error = read_grid(info.filename, grid);
        if (error == EXITCODE_FILE_NOT_FOUND) {
            char* path = (char*)malloc(COMMAND_MAX_LENGTH * sizeof(char));
            strcpy(path, PATH_GRID_FOLDER);
            strcpy(path + strlen(path), info.filename);
            error = read_grid(path, grid);
            free(path);
            if (error == EXITCODE_FILE_NOT_FOUND) {
                perror("create_grid: read_grid: ");
                return EXITCODE_FILE_NOT_FOUND;
            }
        }
        if (error == EXITCODE_WRONG_WIDTH) {
            fprintf(stderr, "%s: %s\n", info.filename, ERROR_MESSAGE_WRONG_WIDTH);
            return EXITCODE_WRONG_WIDTH;
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

void send_grid_layer(Grid grid, int start_row, int end_row, int from, int to, MessageTag tag) {
    int grid_layer_size;
    int* grid_layer_arr = serialize_grid_layer(grid, start_row, end_row, &grid_layer_size);
    int status = MPI_Send(grid_layer_arr, grid_layer_size, MPI_INT, to, tag, MPI_COMM_WORLD);
    if (status != MPI_SUCCESS) {
        fprintf(stderr, "%s to %d\n", ERROR_MESSAGE_CANNOT_SEND, to);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
    free(grid_layer_arr);
}

void receive_grid_layer(Grid* grid, int start_row, int end_row, int from, MessageTag tag) {
    int layer_arr_size = (end_row - start_row) * grid->width;
    MPI_Status status;
    int* layer_arr = (int*)malloc(layer_arr_size * sizeof(int));
    MPI_Recv(layer_arr, layer_arr_size, MPI_INT, from, tag, MPI_COMM_WORLD, &status);
    deserialize_grid_layer(grid, start_row, layer_arr, layer_arr_size);
    free(layer_arr);
}

void broadcast_tag(int from, MessageTag tag, MPI_Comm comm) {
    MPI_Request request;
    int send_status = MPI_Ibcast(&tag, 1, MPI_INT, from, comm, &request);
    if (send_status != MPI_SUCCESS) {
        fprintf(stderr, "%d: %s\n", from, ERROR_MESSAGE_CANNOT_SEND);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
    MPI_Wait(&request, MPI_STATUS_IGNORE);
}

// void broadcast_tag(int proc_quantity, int from, MessageTag tag) {
//  int send_status;
//  for (int i = 0; i < proc_quantity; ++i) {
//      if (i == from) {
//          continue;
//      }
//      send_status = MPI_Send(&i, 0, MPI_INT, i, tag, MPI_COMM_WORLD);
//      if (send_status != MPI_SUCCESS) {
//          fprintf(stderr, "%d: %s to %d\n", from, ERROR_MESSAGE_CANNOT_SEND, i);
//          MPI_Abort(MPI_COMM_WORLD, -1);
//      }
//  }
// }
