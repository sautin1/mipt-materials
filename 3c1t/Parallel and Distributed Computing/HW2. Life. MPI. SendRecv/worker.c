#include "worker.h"

const int ITER_DIFF = 5;

WorkerDuty duty;
Grid grid;
Grid grid_next;
int rank;
int proc_quantity;
int neighbor_up;
int neighbor_down;
double start_time;
double end_time;

void update_grid_rows() {
    for (int row = 1; row < grid.height-1; ++row) {
        for (int column = 0; column < grid.width; ++column) {
            grid_next.states[row][column] = update_cell(grid, row, column);
        }
    }
}

// function for apprentice
void finalize_run(int iter_quantity) {
    MPI_Status status;
    int tmp;
    for (int i = 2; i < proc_quantity; ++i) {
        MPI_Recv(&tmp, 0, MPI_INT, MPI_ANY_SOURCE, FINISH, MPI_COMM_WORLD, &status);
        // fprintf(stderr, "%d: got FINISH from %d\n", rank, status.MPI_SOURCE);
        send_state(WORKER_READY, status.MPI_SOURCE, FINISH);
    }
    end_time = MPI_Wtime();
    double work_time = end_time - start_time;
    fprintf(stderr, "workers completed %d iterations in %f sec\n", iter_quantity, work_time);
}

void notify_end_work() {
    int status;
    status = MPI_Send(&status, 0, MPI_INT, APPRENTICE_ID, FINISH, MPI_COMM_WORLD);
    if (status != MPI_SUCCESS) {
        fprintf(stderr, "%d: %s to %d\n", rank, ERROR_MESSAGE_CANNOT_SEND, APPRENTICE_ID);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
    // fprintf(stderr, "%d: sent FINISH to apprentice\n", rank);
    MPI_Status mpi_status;
    MPI_Recv(&status, 1, MPI_INT, APPRENTICE_ID, FINISH, MPI_COMM_WORLD, &mpi_status);
    if (status == WORKER_BUSY) {
        notify_end_work();
    }
}

void send_state(WorkerState state, int to, MessageTag tag) {
    int status = MPI_Send(&state, 1, MPI_INT, to, tag, MPI_COMM_WORLD);
    if (status != MPI_SUCCESS) {
        fprintf(stderr, "%d: %s to %d\n", rank, ERROR_MESSAGE_CANNOT_SEND, to);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
}

void stop_others(int max_iteration, int* iter_quantity) {
    for (int worker = 1; worker < proc_quantity; ++worker) {
        if (rank != worker) {
            int status = MPI_Send(iter_quantity, 0, MPI_INT, worker, STOP, MPI_COMM_WORLD);
            if (status != MPI_SUCCESS) {
                fprintf(stderr, "%d: %s to %d\n", rank, ERROR_MESSAGE_CANNOT_SEND, worker);
                MPI_Abort(MPI_COMM_WORLD, -1);
            }
        }
    }
    for (int worker = 1; worker < proc_quantity; ++worker) {
        if (rank != worker) {
            MPI_Status status;
            int worker_iter;
            MPI_Recv(&worker_iter, 1, MPI_INT, worker, STOP, MPI_COMM_WORLD, &status);
            max_iteration = (max_iteration < worker_iter) ? worker_iter : max_iteration;
        }
    }
    *iter_quantity = max_iteration + 1;
    for (int worker = 1; worker < proc_quantity; ++worker) {
        if (rank != worker) {
            int status = MPI_Send(iter_quantity, 1, MPI_INT, worker, STOP, MPI_COMM_WORLD);
            if (status != MPI_SUCCESS) {
                fprintf(stderr, "%d: %s to %d\n", rank, ERROR_MESSAGE_CANNOT_SEND, worker);
                MPI_Abort(MPI_COMM_WORLD, -1);
            }
        }
    }
}

void try_exchange_rows(int neighbor, int is_up, int iter, int* iter_quantity) {
    int layer_size;
    int send_row_index = (is_up == 1) ? 1 : (grid.height - 2);
    int* send_row_arr = serialize_grid_layer(grid, send_row_index, send_row_index + 1, 
        &layer_size);

    MPI_Status status;
    int* recv_row_arr = (int*)malloc(layer_size * sizeof(int));
    MPI_Sendrecv(send_row_arr, layer_size, MPI_INT, neighbor, EXCHANGE,
                 recv_row_arr, layer_size, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG,
                 MPI_COMM_WORLD, &status);
    if (status.MPI_TAG != EXCHANGE) {
        if (status.MPI_TAG == STOP_ALL) {
            stop_others(iter, iter_quantity);
        } else if (status.MPI_TAG == STOP) {
            int send_status = MPI_Send(&iter, 1, MPI_INT, APPRENTICE_ID, STOP, MPI_COMM_WORLD);
            if (send_status != MPI_SUCCESS) {
                fprintf(stderr, "%d: %s to %d\n", rank, ERROR_MESSAGE_CANNOT_SEND, APPRENTICE_ID);
                MPI_Abort(MPI_COMM_WORLD, -1);
            }
            MPI_Recv(iter_quantity, 1, MPI_INT, APPRENTICE_ID, STOP, MPI_COMM_WORLD, &status);
        } else if (status.MPI_TAG == TEST) {
            send_state(WORKER_BUSY, MASTER_ID, TEST);
        } else if (status.MPI_TAG == FINISH) {
            send_state(WORKER_BUSY, status.MPI_SOURCE, FINISH);
            // fprintf(stderr, "%d: rejected FINISH from %d\n", rank, status.MPI_SOURCE);
        }
        MPI_Recv(recv_row_arr, layer_size, MPI_INT, MPI_ANY_SOURCE, EXCHANGE, 
            MPI_COMM_WORLD, &status);
    }
    int row = 0;
    if ((neighbor_down == neighbor_up && is_up) || (neighbor_down != neighbor_up 
        && status.MPI_SOURCE == neighbor_down)) {
        // these conditions are needed to deal with cases when there are 
        // only 2 workers (neighbor_up == neighbor_down)
        row = grid.height - 1;
    }
    deserialize_grid_layer(&grid, row, recv_row_arr, layer_size);
    free(send_row_arr);
    free(recv_row_arr);
}

void worker_run(int iter_quantity) {
    if (rank == 1) {
        start_time = MPI_Wtime();
    }
    for (int iter = 0; iter < iter_quantity; ++iter) {
        try_exchange_rows(neighbor_up, 1, iter, &iter_quantity);
        // fprintf(stderr, "%d: %d.%d - %d\n", rank, iter, 1, iter_quantity);
        try_exchange_rows(neighbor_down, 0, iter, &iter_quantity);
        // fprintf(stderr, "%d: %d.%d - %d\n", rank, iter, 2, iter_quantity);

        update_grid_rows();
        Grid grid_tmp = grid;
        grid = grid_next;
        grid_next = grid_tmp;
    }
    // fprintf(stderr, "%d: finished\n", rank);
    if (rank == APPRENTICE_ID) {
        finalize_run(iter_quantity);
    } else {
        notify_end_work();
    }
}

void receive_grid() {
    int duty_array[3];
    MPI_Status status;
    MPI_Recv(duty_array, 3, MPI_INT, MASTER_ID, DUTY, MPI_COMM_WORLD, &status);
    duty.start_row = duty_array[0];
    duty.end_row   = duty_array[1];
    empty_grid(duty.end_row - duty.start_row + 2, duty_array[2], &grid);
    receive_grid_layer(&grid, 1, grid.height - 1, MASTER_ID, GRID);
}

void launch_worker(int arg1) {
    rank = arg1;
    int iter_quantity;

    while (1) {
        int tmp;
        MPI_Status status;
        MPI_Recv(&tmp, 0, MPI_INT, MASTER_ID, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
        if (status.MPI_TAG == START) {
            MPI_Recv(&proc_quantity, 1, MPI_INT, MASTER_ID, DUTY, MPI_COMM_WORLD, &status);
            if (rank > proc_quantity) {
                break;
            }
            ++proc_quantity;
            neighbor_down = (rank < proc_quantity - 1) ? (rank + 1) : 1;
            neighbor_up   = (rank > 1)                 ? (rank - 1) : proc_quantity - 1;
            receive_grid();
            empty_grid(grid.height, grid.width, &grid_next);
        } else if (status.MPI_TAG == STATUS) {
            send_grid_layer(grid, 1, grid.height-1, rank, MASTER_ID, STATUS);
        } else if (status.MPI_TAG == RUN) {
            MPI_Recv(&iter_quantity, 1, MPI_INT, MASTER_ID, ITER, MPI_COMM_WORLD, &status);
            worker_run(iter_quantity);
        } else if (status.MPI_TAG == QUIT) {
            delete_grid(grid);
            delete_grid(grid_next); 
            break;
        } else if (status.MPI_TAG == TEST) {
            send_state(WORKER_READY, MASTER_ID, TEST);
        } else if (status.MPI_TAG != STOP_ALL) {
            fprintf(stderr, "%d: %s\n", rank, ERROR_MESSAGE_WRONG_TAG);
        }
    }
}