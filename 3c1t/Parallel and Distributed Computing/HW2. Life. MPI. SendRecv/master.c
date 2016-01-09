#include "master.h"

Grid grid;
int worker_quantity;

WorkerState check_worker_state(int worker) {
    int status;
    status = MPI_Send(&status, 0, MPI_INT, worker, TEST, MPI_COMM_WORLD);
    if (status != MPI_SUCCESS) {
        fprintf(stderr, "MASTER: %s to %d\n", ERROR_MESSAGE_CANNOT_SEND, worker);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
    MPI_Status mpi_status;
    MPI_Recv(&status, 1, MPI_INT, worker, TEST, MPI_COMM_WORLD, &mpi_status);
    // fprintf(stderr, "MASTER: %d is %s\n", worker, (status == 0) ? "ready" : "busy");
    return status;
}

ssize_t is_workers_busy() {
    ssize_t is_busy = 0;
    for (int i = 0; i < worker_quantity; ++i) {
        WorkerState worker_state = check_worker_state(i + 1);
        is_busy += (worker_state == WORKER_BUSY) ? 1 : 0;
        if (is_busy) {
            break;
        }
    }
    return is_busy;
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

void send_worker_duty(WorkerDuty duty, int worker) {
    int duty_arr[3] = {duty.start_row, duty.end_row, grid.width};
    int status = MPI_Send(duty_arr, 3, MPI_INT, worker, DUTY, MPI_COMM_WORLD);
    if (status != MPI_SUCCESS) {
        fprintf(stderr, "MASTER: %s to %d\n", ERROR_MESSAGE_CANNOT_SEND, worker);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
}

void execute_start(char* command, WorkerDuty** worker_duties, int* is_started) {
    CommandStartInfo info;
    if (!parse_start_command(command, &info) && !create_grid(info, &grid)) {
        int new_worker_quantity = (info.worker_quantity < worker_quantity) ? 
            info.worker_quantity : worker_quantity;
        new_worker_quantity = (grid.height < new_worker_quantity) ? 
            grid.height : new_worker_quantity;
        distribute_duties(grid, new_worker_quantity, worker_duties);
        broadcast_tag(worker_quantity + 1, MASTER_ID, START);
        for (int i = 0; i < worker_quantity; ++i) {
            int status = MPI_Send(&new_worker_quantity, 1, MPI_INT, i+1, DUTY, MPI_COMM_WORLD);
            if (status != MPI_SUCCESS) {
                fprintf(stderr, "MASTER: %s to %d\n", ERROR_MESSAGE_CANNOT_SEND, i+1);
                MPI_Abort(MPI_COMM_WORLD, -1);
            }
        }
        worker_quantity = new_worker_quantity;
        for (int i = 0; i < worker_quantity; ++i) {
            WorkerDuty duty = (*worker_duties)[i];
            send_worker_duty(duty, i+1);
            send_grid_layer(grid, duty.start_row, duty.end_row, MASTER_ID, i+1, GRID);
        }
        *is_started = 1;
    }
}

void execute_status(WorkerDuty* duties) {
    broadcast_tag(worker_quantity + 1, MASTER_ID, STATUS);
    for (int i = 0; i < worker_quantity; ++i) {
        receive_grid_layer(&grid, duties[i].start_row, duties[i].end_row, i+1, STATUS);
    }
    // FILE* res_file = fopen(FILENAME_RESULTS, "w");
    /**/FILE* res_file = stdout;
    if (!print_grid(res_file, grid)) {
        // fclose(res_file);
    }
}

void execute_run(int iter_quantity) {
    broadcast_tag(worker_quantity + 1, MASTER_ID, RUN);
    for (int i = 0; i < worker_quantity; ++i) {
        int status = MPI_Send(&iter_quantity, 1, MPI_INT, i+1, ITER, MPI_COMM_WORLD);
        if (status != MPI_SUCCESS) {
            fprintf(stderr, "MASTER: %s to %d\n", ERROR_MESSAGE_CANNOT_SEND, i+1);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
    }
}

void execute_stop() {
    int status;
    status = MPI_Send(&status, 0, MPI_INT, APPRENTICE_ID, STOP_ALL, MPI_COMM_WORLD);
    if (status != MPI_SUCCESS) {
        fprintf(stderr, "MASTER: %s to %d\n", ERROR_MESSAGE_CANNOT_SEND, 1);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
}

void execute_quit() {
    broadcast_tag(worker_quantity + 1, MASTER_ID, QUIT);
    delete_grid(grid);
}

int launch_master(int arg) {
    worker_quantity = arg;
    WorkerDuty* worker_duties = NULL;
    int is_started      = 0;
    int iter_quantity   = 0;

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