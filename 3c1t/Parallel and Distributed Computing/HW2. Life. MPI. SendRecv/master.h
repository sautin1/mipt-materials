#include <mpi.h>
#include <stdio.h>

#include "commands.h"
#include "life_grid.h"

WorkerState check_worker_state(int worker);
ssize_t is_workers_busy();

ssize_t check_before_start(int is_started);
ssize_t check_before_run(int is_started);
ssize_t check_before_stop(int is_started);
ssize_t check_before_quit();
ssize_t check_before_status(int is_started);

void send_worker_duty(WorkerDuty duty, int worker);

void execute_start(char* command, WorkerDuty** worker_duties, int* is_started);
void execute_status(WorkerDuty* duties);
void execute_run(int iter_quantity);
void execute_stop();
void execute_quit();

int launch_master(int arg);