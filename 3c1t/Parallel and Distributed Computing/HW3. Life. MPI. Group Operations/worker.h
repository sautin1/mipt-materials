#pragma once

#include <mpi.h>
#include <stdio.h>

#include "commands.h"
#include "life_grid.h"

extern const int APPRENTICE_WORK_ID;

void update_grid_rows();

void finalize_run(int iter_quantity);
void notify_end_work();
void send_state(WorkerState state);

int  exchange_rows(int is_up, int iter_quantity);
void worker_run(int iter_quantity, MPI_Request* head_request, int* head_tag);

void receive_grid();
void gather_worker_grids_send();

void launch_worker(int arg1, int arg2, MPI_Comm arg3, MPI_Comm arg4);
