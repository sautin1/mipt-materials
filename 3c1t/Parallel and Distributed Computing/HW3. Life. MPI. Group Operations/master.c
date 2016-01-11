#include "master.h"

Grid grid;
int worker_quantity;
MPI_Comm head_comm;

ssize_t is_workers_busy() {
    int status;
    status = MPI_Send(&status, 0, MPI_INT, APPRENTICE_ID, TAG_TEST, MPI_COMM_WORLD);
    if (status != MPI_SUCCESS) {
        fprintf(stderr, "MASTER: %s to %d\n", ERROR_MESSAGE_CANNOT_SEND, APPRENTICE_ID);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
    MPI_Recv(&status, 1, MPI_INT, APPRENTICE_ID, TAG_TEST, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    return status;
}

ssize_t check_before_start(int is_started) {
    if (is_started) {
        fprintf(stderr, "%s\n", ERROR_MESSAGE_ALREADY_STARTED);
        return -1;
    }
    return 0;
}

ssize_t check_before_run(int is_started) {
    if (!is_started) {
        fprintf(stderr, "%s\n", ERROR_MESSAGE_NOT_STARTED);
        return -1;
    }

    if (is_workers_busy()) {
        fprintf(stderr, "%s\n", ERROR_MESSAGE_NOT_STOPPED);
        return -1;  
    }
    return 0;
}

ssize_t check_before_stop(int is_started) {
    if (!is_started) {
        fprintf(stderr, "%s\n", ERROR_MESSAGE_NOT_STARTED);
        return -1;
    }
    return 0;
}

ssize_t check_before_quit() {
    if (is_workers_busy()) {
        fprintf(stderr, "%s\n", ERROR_MESSAGE_NOT_STOPPED);
        return -1;
    }
    return 0;
}

ssize_t check_before_status(int is_started) {
    if (!is_started) {
        fprintf(stderr, "%s\n", ERROR_MESSAGE_NOT_STARTED);
        return -1;
    }
    if (is_workers_busy()) {
        fprintf(stderr, "%s\n", ERROR_MESSAGE_NOT_STOPPED);
        return -1;
    }
    return 0;
}

void scatter_worker_duties(WorkerDuty* worker_duties) {
    int* duty_arr = (int*)malloc((worker_quantity + 1) * 3 * sizeof(int));
    int index = 3;
    for (int i = 0; i < worker_quantity; ++i) {
        WorkerDuty duty = worker_duties[i];
        duty_arr[index++] = duty.start_row;
        duty_arr[index++] = duty.end_row;
        duty_arr[index++] = grid.width;
    }
    int duty_recv_arr[3];
    MPI_Scatter(duty_arr, 3, MPI_INT, duty_recv_arr, 3, MPI_INT, MASTER_ID, MPI_COMM_WORLD);
    free(duty_arr);
}

void scatter_worker_grids(WorkerDuty* worker_duties) {
    int* grid_arr = (int*)malloc(grid.width * grid.height * sizeof(int));
    int* sizes = (int*)malloc((worker_quantity + 1) * sizeof(int));
    int* disps = (int*)malloc((worker_quantity + 1) * sizeof(int));
    sizes[0] = 0;
    disps[0] = 0;
    for (int i = 1; i < worker_quantity + 1; ++i) {
        disps[i] = disps[i-1] + sizes[i-1];
        WorkerDuty duty = worker_duties[i-1];
        int* layer = serialize_grid_layer(grid, duty.start_row, duty.end_row, &(sizes[i]));
        memcpy(grid_arr + disps[i], layer, sizes[i] * sizeof(int));
    }
    int grid_recv;

    MPI_Scatterv(grid_arr, sizes, disps, MPI_INT, &grid_recv, 0, MPI_INT, MASTER_ID, MPI_COMM_WORLD);
    free(grid_arr);
    free(sizes);
    free(disps);
}

void gather_worker_grids_recv(WorkerDuty* worker_duties) {
    int* grid_arr = (int*)malloc(grid.width * grid.height * sizeof(int));
    int* sizes = (int*)malloc((worker_quantity + 1) * sizeof(int));
    int* disps = (int*)malloc((worker_quantity + 1) * sizeof(int));
    sizes[0] = 0;
    disps[0] = 0;
    for (int i = 1; i < worker_quantity + 1; ++i) {
        WorkerDuty duty = worker_duties[i-1];
        sizes[i] = (duty.end_row - duty.start_row) * grid.width;
        disps[i] = disps[i-1] + sizes[i-1];
    }

    int grid_send;
    MPI_Gatherv(&grid_send, 0, MPI_INT, grid_arr, sizes, disps, MPI_INT, 
        MASTER_ID, MPI_COMM_WORLD);
    deserialize_grid_layer(&grid, 0, grid_arr, grid.height * grid.width);
    free(grid_arr);
    free(sizes);
    free(disps);
}

void execute_start(char* command, WorkerDuty** worker_duties, int* is_started) {
    CommandStartInfo info;
    if (!parse_start_command(command, &info) && !create_grid(info, &grid)) {
        if (grid.height < worker_quantity) {
            fprintf(stderr, "%s\n", ERROR_MESSAGE_SMALL_GRID);
            return;
        }

        distribute_duties(grid, worker_quantity, worker_duties);
        // broadcast_tag(MASTER_ID, TAG_START, MPI_COMM_WORLD);
        broadcast_tag(worker_quantity + 1, MASTER_ID, TAG_START);

        scatter_worker_duties(*worker_duties);
        scatter_worker_grids(*worker_duties);
        *is_started = 1;
    }
}

void execute_status(WorkerDuty* duties) {
    // broadcast_tag(MASTER_ID, TAG_STATUS, MPI_COMM_WORLD);
    broadcast_tag(worker_quantity + 1, MASTER_ID, TAG_STATUS);
    gather_worker_grids_recv(duties);

    // FILE* res_file = fopen(FILENAME_RESULTS, "w");
    /**/FILE* res_file = stdout;
    if (!print_grid(res_file, grid)) {
        // fclose(res_file);
    }
}

void execute_run(int iter_quantity) {
    // broadcast_tag(MASTER_ID, TAG_RUN, MPI_COMM_WORLD);
    broadcast_tag(worker_quantity + 1, MASTER_ID, TAG_RUN);
    int send_status = MPI_Bcast(&iter_quantity, 1, MPI_INT, MASTER_ID, MPI_COMM_WORLD);
    if (send_status != MPI_SUCCESS) {
        fprintf(stderr, "%d: %s\n", MASTER_ID, ERROR_MESSAGE_CANNOT_SEND);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
}

void execute_stop() {
    int status;
    status = MPI_Send(&status, 0, MPI_INT, APPRENTICE_ID, TAG_STOP_ALL, MPI_COMM_WORLD);
    if (status != MPI_SUCCESS) {
        fprintf(stderr, "MASTER: %s to %d\n", ERROR_MESSAGE_CANNOT_SEND, 1);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
}

void execute_quit() {
    // broadcast_tag(MASTER_ID, TAG_QUIT, MPI_COMM_WORLD);
    broadcast_tag(worker_quantity + 1, MASTER_ID, TAG_QUIT);

    delete_grid(grid);
}

int launch_master(int arg, MPI_Comm arg2) {
    worker_quantity = arg;
    WorkerDuty* worker_duties = NULL;
    int is_started      = 0;
    int iter_quantity   = 0;
    head_comm = arg2;

    char* command = (char*)malloc(COMMAND_MAX_LENGTH * sizeof(char));
    while (1) {
        printf("> ");
        command = fgets(command, COMMAND_MAX_LENGTH, stdin);
        normalize_command(command);
        if (strncmp("start", command, 5) == 0) {
            if (!check_before_start(is_started)) {
                execute_start(command, &worker_duties, &is_started);
            }
        } else if (strncmp("run", command, 3) == 0) {
            if (!check_before_run(is_started) && !parse_run_command(command, &iter_quantity)) {
                execute_run(iter_quantity);
            }
        } else if (strcmp("status", command) == 0) {
            if (!check_before_status(is_started)) {
                execute_status(worker_duties);
            }
        } else if (strcmp("stop", command) == 0) {
            if (!check_before_stop(is_started)) {
                execute_stop();
            }
        } else if (strcmp("help", command) == 0) {
            print_help();
        } else if (strcmp("quit", command) == 0) {
            if (!check_before_quit()) {
                execute_quit();
                free(worker_duties);
                break;
            }
        } else {
            if (command[0] != 0) {
                fprintf(stderr, "%s\n", ERROR_MESSAGE_WRONG_COMMAND);
            }
        }
    }
    if (command) {
        free(command);
    }
    return 0;
}