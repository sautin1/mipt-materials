#define _POSIX_C_SOURCE 199309L
#include <pthread.h>
#include <stdlib.h>
#include <time.h>       // nanosleep, time
#include <sys/time.h>   // gettimeofday
#include <unistd.h>

#include "commands.h"
#include "life_grid.h"

const long MANAGER_SLEEP_NANOSEC = 100000000;

Grid grid;
Grid grid_next;

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cond_var = PTHREAD_COND_INITIALIZER;
int thread_iterations_left  = 0;
int flag_stop               = 0;
int flag_stop_internal      = 0;    // changed only in update_grid_rows (maximum once per launch)
int worker_quantity         = 0;
struct timeval start_time;
struct timeval   end_time;

// function for workers
void* update_grid_rows(void* arg) {
    WorkerDuty duty = *(WorkerDuty*)arg;
    for (int iter = 0; iter < duty.iter_quantity; ++iter) {
        for (int row = duty.start_row; row < duty.end_row; ++row) {
            for (int column = 0; column < grid.width; ++column) {
                grid_next.states[row][column] = update_cell(grid, row, column);
            }
        }
        // barrier
        pthread_mutex_lock(&mutex);
        --thread_iterations_left;
        if (thread_iterations_left % worker_quantity == 0) {
            // it is the last thread in this iteration
            Grid grid_copy = grid;
            grid = grid_next;
            grid_next = grid_copy;
            flag_stop_internal = flag_stop;
            if (thread_iterations_left == 0 || flag_stop_internal) {
                gettimeofday(&end_time, NULL);
            }
            pthread_cond_broadcast(&cond_var);
        }
        if (thread_iterations_left % worker_quantity > 0) {
            pthread_cond_wait(&cond_var, &mutex);
        }
        pthread_mutex_unlock(&mutex);
        if (flag_stop_internal) {
            break;
        }
    }
    return NULL;
}

void finalize_workers(pthread_t* workers, WorkerDuty* worker_duties) {
    for (int i = 0; i < worker_quantity; ++i) {
        pthread_join(workers[i], NULL);
    }
    free(workers);
    free(worker_duties);
}

// manager function
void* wait_for_workers(void* arg) {
    ManagerInfo info = *(ManagerInfo*)arg;
    pthread_t* workers = info.workers;
    WorkerDuty* worker_duties = info.duties;

    struct timespec sleep_time;
    sleep_time.tv_sec  = 0;
    sleep_time.tv_nsec = MANAGER_SLEEP_NANOSEC;

    while (thread_iterations_left > 0 && !flag_stop) {
        nanosleep(&sleep_time, NULL);
    }
    int iter_done = worker_duties[0].iter_quantity - thread_iterations_left / worker_quantity;
    double work_time = (end_time.tv_sec - start_time.tv_sec);
    work_time += 1e-6 * (end_time.tv_usec - start_time.tv_usec);
    fprintf(stderr, "workers completed %d iterations in %f sec\n", iter_done, work_time);
    finalize_workers(workers, worker_duties);
    thread_iterations_left = 0;
    return NULL;
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

void execute_run(int iter_quantity, int worker_quantity) {
    WorkerDuty* duties = NULL;
    distribute_duties(grid, worker_quantity, iter_quantity, &duties);

    pthread_t* workers = (pthread_t*)malloc(worker_quantity * sizeof(pthread_t));
    pthread_t manager;
    thread_iterations_left = worker_quantity * iter_quantity;
    flag_stop = 0;
    flag_stop_internal = 0;
    ManagerInfo* manager_info = (ManagerInfo*)malloc(sizeof(ManagerInfo));
    manager_info->workers = workers;
    manager_info->duties  = duties;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_create(&manager, &attr, &wait_for_workers, manager_info);
    gettimeofday(&start_time, NULL);
    for (int i = 0; i < worker_quantity; ++i) {
        pthread_create(workers + i, NULL, &update_grid_rows, duties + i);
    }
}

int launch_master() {
    int is_started = 0;

    char* command = (char*)malloc(COMMAND_MAX_LENGTH * sizeof(char));
    while (1) {
        printf("> ");
        if (!fgets(command, COMMAND_MAX_LENGTH, stdin)) {
            break;
        }
        normalize_command(command);
        if (strncmp("start", command, 5) == 0) {
            CommandStartInfo info;
            if (!check_before_start(is_started) && !parse_start_command(command, &info) 
                && !create_grid(info, &grid) && !empty_grid(grid.height, grid.width, &grid_next)) {
                worker_quantity = (info.worker_quantity <= grid.height) ? info.worker_quantity :
                                                                          grid.height;
                is_started = 1;
            }
        } else if (strcmp("status", command) == 0) {
            if (!check_before_status(is_started)) {
                //FILE* res_file = fopen(FILENAME_RESULTS, "w");
                /**/FILE* res_file = stdout;
                if (!print_grid(res_file, grid)) {
                    //fclose(res_file);
                }
            }
        } else if (strncmp("run", command, 3) == 0) {
            int iter_quantity;
            if (!check_before_run(is_started) && !parse_run_command(command, &iter_quantity)) {
                execute_run(iter_quantity, worker_quantity);
            }
        } else if (strcmp("stop", command) == 0) {
            if (!check_before_stop(is_started)) {
                flag_stop = 1;
            }
        } else if (strcmp("quit", command) == 0) {
            if (!check_before_quit()) {
                delete_grid(grid);
                delete_grid(grid_next);
                pthread_cond_destroy(&cond_var);
                pthread_mutex_destroy(&mutex);
                break;
            }
        } else if (strcmp("help", command) == 0) {
            print_help();
        } else {
            if (command[0] != 0) {
                fprintf(stderr, "Wrong command\n");
            }
        }
    }
    if (command) {
        free(command);
    }
    return 0;
}

int main() {
    srand(time(NULL));
    launch_master();
    printf("Success!\n");
    return 0;
}
