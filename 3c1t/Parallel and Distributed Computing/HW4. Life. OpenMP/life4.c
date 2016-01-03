#include <omp.h>
#include <stdlib.h>
#include <time.h>       // nanosleep, time
#include <unistd.h>

#include "commands.h"
#include "life_grid.h"

Grid grid;
Grid grid_next;

int thread_iterations_left  = 0;
int flag_stop               = 0;
int flag_stop_internal      = 0;    // changed only in update_grid_rows (maximum once per launch)

// function for workers
void update_grid_rows(WorkerDuty* duties) {
    int id = omp_get_thread_num();
    WorkerDuty duty = duties[id];
    for (int iter = 0; iter < duty.iter_quantity; ++iter) {
        for (int row = duty.start_row; row < duty.end_row; ++row) {
            for (int column = 0; column < grid.width; ++column) {
                grid_next.states[row][column] = update_cell(grid, row, column);
            }
        }
        #pragma omp atomic
            --thread_iterations_left;
        #pragma omp barrier
        #pragma omp single
        {
            Grid grid_copy = grid;
            grid = grid_next;
            grid_next = grid_copy;
            flag_stop_internal = flag_stop;
        }
        if (flag_stop_internal) {
            break;
        }
    }
}

void finalize_run(WorkerDuty** worker_duties) {
    free(*worker_duties);
    *worker_duties = NULL;
    thread_iterations_left = 0;
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
    if (thread_iterations_left > 0) {
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
    if (thread_iterations_left > 0) {
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
    if (thread_iterations_left > 0) {
        fprintf(stderr, "%s\n", ERROR_MESSAGE_NOT_STOPPED);
        return -1;
    }
    return 0;
}

void execute_run(int iter_quantity, int worker_quantity, int thread_id, WorkerDuty** duties) {
    #pragma omp master
    {
        distribute_duties(grid, worker_quantity, iter_quantity, duties);
        thread_iterations_left = worker_quantity * iter_quantity;
        flag_stop = 0;
        flag_stop_internal = 0;
    }
    omp_set_nested(1);
    #pragma omp barrier
    if (thread_id == 1) {
        double start_time = omp_get_wtime();
        #pragma omp parallel num_threads(worker_quantity)
        {
            update_grid_rows(*duties);
        }
        double end_time = omp_get_wtime();
        double work_time = end_time - start_time;
        int iter_done = iter_quantity - thread_iterations_left / worker_quantity;
        fprintf(stderr, "workers completed %d iterations in %f sec\n", iter_done, work_time);
        finalize_run(duties);
    }
}

int launch_master() {
    WorkerDuty* worker_duties = NULL;
    int is_started      = 0;
    int iter_quantity   = 0;
    int worker_quantity = 0;

    char* command = (char*)malloc(COMMAND_MAX_LENGTH * sizeof(char));
    #pragma omp parallel num_threads(2) 
    {
        while (1) {
            #pragma omp master
            {
                printf("> ");
                command = fgets(command, COMMAND_MAX_LENGTH, stdin);
                normalize_command(command);
            }
            int thread_id = omp_get_thread_num();
            if (strncmp("run", command, 3) == 0 || strcmp("quit", command) == 0 || thread_id > 0) {
                if (thread_id == 0) {
                    if ((strncmp("run", command, 3) == 0 && (check_before_run(is_started)
                        || parse_run_command(command, &iter_quantity)))
                        || (strcmp("quit", command) == 0 && check_before_quit())) {
                            continue; // error occured
                    }
                }
                #pragma omp barrier
                if (strncmp("run", command, 3) == 0) {
                    execute_run(iter_quantity, worker_quantity, thread_id, &worker_duties);
                } else if (strcmp("quit", command) == 0) {
                    #pragma omp master
                    {
                        delete_grid(grid);
                        delete_grid(grid_next);
                    }
                    break;
                }
            } else if (strncmp("start", command, 5) == 0) {
                #pragma omp master
                {
                    if (!check_before_start(is_started)) {
                        CommandStartInfo info;
                        if (!parse_start_command(command, &info) && !create_grid(info, &grid) && 
                            !empty_grid(grid.height, grid.width, &grid_next)) {
                            worker_quantity = (info.worker_quantity <= grid.height) ? info.worker_quantity :
                                                                                      grid.height;
                            is_started = 1;
                        }
                    }
                }
            } else if (strcmp("status", command) == 0) {
                #pragma omp master
                {
                    if (!check_before_status(is_started)) {
                        //FILE* res_file = fopen(FILENAME_RESULTS, "w");
                        /**/FILE* res_file = stdout;
                        if (!print_grid(res_file, grid)) {
                            // fclose(res_file);
                        }
                    }
                }
            } else if (strcmp("stop", command) == 0) {
                #pragma omp master
                {
                    if (!check_before_stop(is_started)) {
                        flag_stop = 1;
                    }
                }
            } else if (strcmp("help", command) == 0) {
                #pragma omp master
                {
                    print_help();
                }
            } else {
                #pragma omp master
                {
                    if (command[0] != 0) {
                        fprintf(stderr, "%s\n", ERROR_MESSAGE_WRONG_COMMAND);
                    }
                }
            }
        }
    }
    if (command) {
        free(command);
    }
    return 0;
}

int main() {
    #ifndef _OPENMP
        fprintf(stderr, "OpenMP is not supported!\n");
        return -1;
    #endif

    srand(time(NULL));
    launch_master();
    printf("Success!\n");
    return 0;
}
