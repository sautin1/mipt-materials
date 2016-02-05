#include "worker.h"

const int APPRENTICE_WORK_ID = 0;

WorkerDuty duty;
Grid grid;
Grid grid_next;
int rank, head_rank;
int proc_quantity;
int neighbor_up;
int neighbor_down;
double start_time;
double end_time;
MPI_Comm worker_comm;
MPI_Comm head_comm;

void update_grid_rows() {
    for (int row = 1; row < grid.height-1; ++row) {
        for (int column = 0; column < grid.width; ++column) {
            grid_next.states[row][column] = update_cell(grid, row, column);
        }
    }
}

void finalize_run(int iter_quantity) {
    int send_result = 1;
    int result;
    MPI_Reduce(&send_result, &result, 1, MPI_INT, MPI_LAND, APPRENTICE_WORK_ID, worker_comm);
    end_time = MPI_Wtime();
    double work_time = end_time - start_time;
    fprintf(stderr, "workers completed %d iterations in %f sec\n", iter_quantity, work_time);
}

void notify_end_work() {
    int send_result = 1;
    int status;
    status = MPI_Reduce(&send_result, &status, 1, MPI_INT, MPI_LAND, 
                        APPRENTICE_WORK_ID, worker_comm);
    if (status != MPI_SUCCESS) {
        fprintf(stderr, "%d: %s to %d\n", rank, ERROR_MESSAGE_CANNOT_SEND, APPRENTICE_ID);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
}

void send_state(WorkerState state) {
    int status = MPI_Bcast(&state, 1, MPI_INT, head_rank, head_comm);
    if (status != MPI_SUCCESS) {
        fprintf(stderr, "%d: %s to %d\n", rank, ERROR_MESSAGE_CANNOT_SEND, MASTER_ID);
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

void worker_run(int iter_quantity, MPI_Request* head_request, int* head_tag) {
    int master_head_rank = 1 - head_rank;
    if (rank == APPRENTICE_ID) {
        start_time = MPI_Wtime();
    }
    int is_stopped = 0;
    for (int iter = 0; iter < iter_quantity; ++iter) {
        if (rank == APPRENTICE_ID && !is_stopped) {
            // ignore master messages if already trying to stop
            int got_message = 0;
            MPI_Test(head_request, &got_message, MPI_STATUS_IGNORE);
            if (got_message) {
                if (*head_tag == TAG_STOP_ALL) {
                    is_stopped = 1;
                    iter_quantity = iter + (proc_quantity - 1) / 2 + 1;
                } else {
                    if (*head_tag == TAG_TEST) {
                        send_state(WORKER_BUSY);
                    }
                    MPI_Ibcast(head_tag, 1, MPI_INT, master_head_rank, head_comm, head_request);
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
        if (is_stopped) {
            // should be here, since we ignore master messages if already trying to stop
            MPI_Ibcast(head_tag, 1, MPI_INT, master_head_rank, head_comm, head_request);
        }
        finalize_run(iter_quantity);
    } else {
        notify_end_work();
    }
}

void receive_grid() {
    int duty_array[3];
    MPI_Scatter(duty_array, 3, MPI_INT, duty_array, 3, MPI_INT, MASTER_ID, MPI_COMM_WORLD);
    duty.start_row = duty_array[0];
    duty.end_row   = duty_array[1];
    empty_grid(duty.end_row - duty.start_row + 2, duty_array[2], &grid);

    int layer_arr_size = (duty.end_row - duty.start_row) * grid.width;
    int* layer_arr = (int*)malloc(layer_arr_size * sizeof(int));
    MPI_Scatterv(duty_array, duty_array, duty_array, MPI_INT, 
        layer_arr, layer_arr_size, MPI_INT, MASTER_ID, MPI_COMM_WORLD);
    deserialize_grid_layer(&grid, 1, layer_arr, layer_arr_size);
    free(layer_arr);
}

void gather_worker_grids_send() {
    int grid_layer_size;
    int* grid_layer_arr = serialize_grid_layer(grid, 1, grid.height - 1, &grid_layer_size);
    MPI_Gatherv(grid_layer_arr, grid_layer_size, MPI_INT, 
        &grid_layer_size, &grid_layer_size, &grid_layer_size, MPI_INT, MASTER_ID, MPI_COMM_WORLD);
    free(grid_layer_arr);
}

void launch_worker(int arg1, int arg2, MPI_Comm arg3, MPI_Comm arg4) {
    rank = arg1;
    proc_quantity = arg2;
    worker_comm = arg3;
    head_comm = arg4;
    int iter_quantity;
    int master_head_rank = -1;
    if (rank == APPRENTICE_ID) {
        MPI_Comm_rank(head_comm, &head_rank);
        master_head_rank = 1 - head_rank;
    }

    int request_index = -1;
    MPI_Request requests[2];
    int request_quantity = (rank == APPRENTICE_ID) ? 2 : 1;
    int tags_incoming[2];
    while (1) {
        if (request_index <= 0) {
            MPI_Ibcast(tags_incoming, 1, MPI_INT, MASTER_ID, MPI_COMM_WORLD, requests);
        }
        if (rank == APPRENTICE_ID && request_index != 0) {
            MPI_Ibcast(tags_incoming + 1, 1, MPI_INT, master_head_rank, head_comm, requests + 1);
        }
        MPI_Waitany(request_quantity, requests, &request_index, MPI_STATUS_IGNORE);
        int tag = tags_incoming[request_index];

        if (tag == TAG_START) {
            neighbor_down = (rank < proc_quantity - 1) ? (rank + 1) : 1;
            neighbor_up   = (rank > 1)                 ? (rank - 1) : proc_quantity - 1;
            receive_grid();
            empty_grid(grid.height, grid.width, &grid_next);
        } else if (tag == TAG_STATUS) {
            gather_worker_grids_send();
        } else if (tag == TAG_RUN) {
            MPI_Bcast(&iter_quantity, 1, MPI_INT, MASTER_ID, MPI_COMM_WORLD);
            worker_run(iter_quantity, requests + 1, tags_incoming + 1);
        } else if (tag == TAG_QUIT) {
            delete_grid(grid);
            delete_grid(grid_next); 
            break;
        } else if (tag == TAG_TEST) {
            send_state(WORKER_READY);
        } else if (tag != TAG_STOP_ALL) {
            fprintf(stderr, "%d: %s\n", rank, ERROR_MESSAGE_WRONG_TAG);
        }
    }
}
