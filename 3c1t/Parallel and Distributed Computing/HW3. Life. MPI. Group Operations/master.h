#pragma once

#include <mpi.h>
#include <stdio.h>

#include "commands.h"
#include "life_grid.h"

WorkerState is_workers_busy();

ssize_t check_before_start(int is_started);
ssize_t check_before_run(int is_started);
ssize_t check_before_stop(int is_started);
ssize_t check_before_quit();
ssize_t check_before_status(int is_started);

void scatter_worker_duties(WorkerDuty* worker_duties);
void scatter_worker_grids(WorkerDuty* worker_duties);
void gather_worker_grids_recv(WorkerDuty* worker_duties);

void execute_start(char* command, WorkerDuty** worker_duties, int* is_started);
void execute_status(WorkerDuty* duties);
void execute_run(int iter_quantity);
void execute_stop();
void execute_quit();

// void launch_master(int arg, MPI_Comm arg2);
void launch_master(int arg, MPI_Comm arg2);