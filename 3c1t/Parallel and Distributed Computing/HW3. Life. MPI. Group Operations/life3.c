#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "master.h"
#include "worker.h"

// void create_worker_comm(MPI_Comm* worker_comm) {
//  int excl_ranks = 0;
//  MPI_Group world_group;
//  MPI_Group worker_group;

//  MPI_Comm_group(MPI_COMM_WORLD, &world_group);
//  MPI_Group_excl(world_group, 1, &excl_ranks, &worker_group);

//  MPI_Comm world_comm;

//  MPI_Comm_dup(MPI_COMM_WORLD, &world_comm);
//  MPI_Comm_create(world_comm, worker_group, worker_comm);
// }

// void create_comm(int size, int* incl_ranks, MPI_Comm* res_comm) {
//  MPI_Comm world_comm;
//  MPI_Comm_dup(MPI_COMM_WORLD, &world_comm);

//  MPI_Group world_group;
//  MPI_Comm_group(MPI_COMM_WORLD, &world_group);

//  MPI_Group res_group;
//  MPI_Group_incl(world_group, size, incl_ranks, &res_group);
//  MPI_Comm_create(world_comm, res_group, res_comm);

//  int rank;
//  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
//  // if (incl_ranks[0] == 0) {
//  //  fprintf(stderr, "%d: create comm %d\n", rank, *res_comm);
//  // }
// }

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
    MPI_Comm_split(MPI_COMM_WORLD, rank == 0 ? 0 : 1, rank, &worker_comm);

    MPI_Comm head_comm;
    MPI_Comm_split(MPI_COMM_WORLD, rank <= 1 ? 0 : 1, rank, &head_comm);

    MPI_Comm* pair_comms = (MPI_Comm*)malloc(size * sizeof(MPI_Comm));
    // create_worker_comm(&worker_comm);

    // for (int i = 0; i < size; ++i) {
    //  int incl_ranks[2] = {i, (i < size - 1) ? (i + 1) : 1};
    //  create_comm(2, incl_ranks, pair_comms + i);
    // }
    // head_comm = pair_comms;
    // fprintf(stderr, "%d: %d\n", rank, *head_comm);

    if (size <= 1) {
        fprintf(stderr, "Too few workers\n");
    } else if (rank == 0) {
        launch_master(size - 1, head_comm);
    } else {
        int up_comm_index = (rank > 1) ? rank - 1 : size - 1;
        launch_worker(rank, size, worker_comm, head_comm, 
            pair_comms[rank], pair_comms[up_comm_index]);
    }
    MPI_Finalize();
    free(pair_comms);
    printf("%d: Success!\n", rank);
    return 0;
}
