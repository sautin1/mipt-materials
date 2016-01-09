#include <mpi.h>
#include <stdio.h>
#include <unistd.h> // temporary (uses sleep for testing)

#include "commands.h"
#include "life_grid.h"

extern const int ITER_DIFF;

void update_grid_rows();

void finalize_run(int iter_quantity);

void notify_end_work();
void send_state(WorkerState state, int to, MessageTag tag);
void stop_others(int current_iteration, int* iter_quantity);
void try_exchange_rows(int neighbor, int is_up, int iter, int* iter_quantity);
void worker_run(int iter_quantity);
void receive_grid();

void launch_worker(int arg1);