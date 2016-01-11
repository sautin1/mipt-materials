#pragma once

#include <mpi.h>
#include <stdio.h>
#include <unistd.h> // temporary (uses sleep for testing)

#include "commands.h"
#include "life_grid.h"

void update_grid_rows();

void finalize_run(int iter_quantity);

void notify_end_work();
void send_state(WorkerState state, int to, MessageTag tag);
int  exchange_rows(int is_up, int iter_quantity);
void worker_run(int iter_quantity);
void receive_grid();

void launch_worker(int arg1);