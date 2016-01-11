#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "master.h"
#include "worker.h"

void create_worker_comm(MPI_Comm* worker_comm) {
    int excl_ranks = 0;
    MPI_Group world_group;
    MPI_Group worker_group;
    
    MPI_Comm_group(MPI_COMM_WORLD, &world_group);
    MPI_Group_excl(world_group, 1, &excl_ranks, &worker_group);
    
    MPI_Comm world_comm;

    MPI_Comm_dup(MPI_COMM_WORLD, &world_comm);
    MPI_Comm_create(world_comm, worker_group, worker_comm);
}

void create_head_comm(MPI_Comm* head_comm) {
    int incl_ranks[2] = {0, 1};
    MPI_Group world_group;
    MPI_Group head_group;

    MPI_Comm_group(MPI_COMM_WORLD, &world_group);
    MPI_Group_incl(world_group, 2, incl_ranks, &head_group);

    MPI_Comm world_comm;

    MPI_Comm_dup(MPI_COMM_WORLD, &world_comm);
    MPI_Comm_create(world_comm, head_group, head_comm);
}

int main(int argc, char** argv) {
    srand(time(NULL));
    int status = MPI_Init(&argc, &argv);
    if (status) {
        fprintf(stderr, "MPI is not supported!\n");
        return -1;
    }
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Comm worker_comm;
    MPI_Comm   head_comm;
    create_worker_comm(&worker_comm);
    create_head_comm(&head_comm);

    if (size <= 1) {
        fprintf(stderr, "Too few workers\n");
    } else if (rank == 0) {
        launch_master(size-1, head_comm);
    } else {
        launch_worker(rank, size, worker_comm, head_comm);
    }

    MPI_Finalize();
    printf("%d: Success!\n", rank);
    return 0;
}
