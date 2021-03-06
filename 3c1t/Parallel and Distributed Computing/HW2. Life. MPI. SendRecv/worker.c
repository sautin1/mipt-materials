#include "worker.h"

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

void finalize_run(int iter_quantity) {
    int tmp;
    for (int i = 2; i < proc_quantity; ++i) {
        MPI_Recv(&tmp, 0, MPI_INT, MPI_ANY_SOURCE, TAG_FINISH, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    }
    end_time = MPI_Wtime();
    double work_time = end_time - start_time;
    fprintf(stderr, "workers completed %d iterations in %f sec\n", iter_quantity, work_time);
}

void notify_end_work() {
    int status;
    status = MPI_Send(&status, 0, MPI_INT, APPRENTICE_ID, TAG_FINISH, MPI_COMM_WORLD);
    if (status != MPI_SUCCESS) {
        fprintf(stderr, "%d: %s to %d\n", rank, ERROR_MESSAGE_CANNOT_SEND, APPRENTICE_ID);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
}

void send_state(WorkerState state, int to, MessageTag tag) {
    int status = MPI_Send(&state, 1, MPI_INT, to, tag, MPI_COMM_WORLD);
    if (status != MPI_SUCCESS) {
        fprintf(stderr, "%d: %s to %d\n", rank, ERROR_MESSAGE_CANNOT_SEND, to);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
}

int exchange_rows(int is_up, int iter_quantity) {
    int layer_size;
    int row = (is_up) ? 1 : grid.height - 2;
    int* tmp_buffer = serialize_grid_layer(grid, row, row + 1, &layer_size);
    int* send_row_arr = (int*)malloc((1 + layer_size) * sizeof(int));
    send_row_arr[0] = iter_quantity;
    ++send_row_arr;
    memcpy(send_row_arr, tmp_buffer, layer_size * sizeof(int));
    --send_row_arr;
    free(tmp_buffer);

    int* recv_row_arr = (int*)malloc((1 + layer_size) * sizeof(int));

    int send_neighbor_rank = (is_up) ? neighbor_up : neighbor_down;
    int recv_neighbor_rank = (is_up) ? neighbor_down : neighbor_up;
    MPI_Sendrecv(send_row_arr, layer_size + 1, MPI_INT, send_neighbor_rank, TAG_EXCHANGE,
                 recv_row_arr, layer_size + 1, MPI_INT, recv_neighbor_rank, TAG_EXCHANGE,
                 MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    
    int new_iter_quantity = recv_row_arr[0];
    row = (is_up) ? (grid.height - 1) : 0;
    ++recv_row_arr;
    deserialize_grid_layer(&grid, row, recv_row_arr, layer_size);
    --recv_row_arr;
    free(send_row_arr);
    free(recv_row_arr);
    return new_iter_quantity;
}

void worker_run(int iter_quantity) {
    if (rank == 1) {
        start_time = MPI_Wtime();
    }
    MPI_Request master_request;
    int tmp;
    if (rank == APPRENTICE_ID) {
        MPI_Irecv(&tmp, 0, MPI_INT, MASTER_ID, MPI_ANY_TAG, MPI_COMM_WORLD, &master_request);
    }
    int is_stopped = 0;
    for (int iter = 0; iter < iter_quantity; ++iter) {
        if (rank == APPRENTICE_ID && !is_stopped) {
            // ignore master if already trying to stop
            int is_message = 0;
            MPI_Status status;
            MPI_Test(&master_request, &is_message, &status);
            if (is_message) {
                if (status.MPI_TAG == TAG_STOP_ALL) {
                    is_stopped = 1;
                    iter_quantity = iter + (proc_quantity - 1) / 2 + 1;
                } else {
                    if (status.MPI_TAG == TAG_TEST) {
                        send_state(WORKER_BUSY, MASTER_ID, TAG_TEST);
                    }
                    MPI_Irecv(&tmp, 0, MPI_INT, MASTER_ID, MPI_ANY_TAG, MPI_COMM_WORLD, &master_request);
                }
            }
        }

        for (int is_direction_up = 0; is_direction_up <= 1; ++is_direction_up) {
            int new_iter_quantity = exchange_rows(is_direction_up, iter_quantity);
            if (new_iter_quantity < iter_quantity) {
                is_stopped = 1;
                iter_quantity = new_iter_quantity;
            }
        }

        update_grid_rows();
        Grid grid_tmp = grid;
        grid = grid_next;
        grid_next = grid_tmp;
    }
    if (rank == APPRENTICE_ID) {
        if (!is_stopped) {
            MPI_Cancel(&master_request);
        }
        finalize_run(iter_quantity);
    } else {
        notify_end_work();
    }
}

void receive_grid() {
    int duty_array[3];
    MPI_Recv(duty_array, 3, MPI_INT, MASTER_ID, TAG_DUTY, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    duty.start_row = duty_array[0];
    duty.end_row   = duty_array[1];
    empty_grid(duty.end_row - duty.start_row + 2, duty_array[2], &grid);
    receive_grid_layer(&grid, 1, grid.height - 1, MASTER_ID, TAG_GRID);
}

void launch_worker(int arg1) {
    rank = arg1;
    int iter_quantity;

    while (1) {
        int tmp;
        MPI_Status status;
        MPI_Recv(&tmp, 0, MPI_INT, MASTER_ID, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
        if (status.MPI_TAG == TAG_START) {
            MPI_Recv(&proc_quantity, 1, MPI_INT, MASTER_ID, TAG_DUTY, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            if (rank > proc_quantity) {
                break;
            }
            ++proc_quantity;
            neighbor_down = (rank < proc_quantity - 1) ? (rank + 1) : 1;
            neighbor_up   = (rank > 1)                 ? (rank - 1) : proc_quantity - 1;
            receive_grid();
            empty_grid(grid.height, grid.width, &grid_next);
        } else if (status.MPI_TAG == TAG_STATUS) {
            send_grid_layer(grid, 1, grid.height-1, rank, MASTER_ID, TAG_STATUS);
        } else if (status.MPI_TAG == TAG_RUN) {
            MPI_Recv(&iter_quantity, 1, MPI_INT, MASTER_ID, TAG_ITER, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            worker_run(iter_quantity);
        } else if (status.MPI_TAG == TAG_QUIT) {
            delete_grid(grid);
            delete_grid(grid_next); 
            break;
        } else if (status.MPI_TAG == TAG_TEST) {
            send_state(WORKER_READY, MASTER_ID, TAG_TEST);
        } else if (status.MPI_TAG != TAG_STOP_ALL) {
            fprintf(stderr, "%d: %s\n", rank, ERROR_MESSAGE_WRONG_TAG);
        }
    }
}