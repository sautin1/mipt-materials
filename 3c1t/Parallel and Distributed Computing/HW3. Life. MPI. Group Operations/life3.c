#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "master.h"
#include "worker.h"

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

    if (size <= 1) {
        fprintf(stderr, "Too few workers\n");
    } else if (rank == 0) {
        launch_master(size - 1, head_comm);
    } else {
        launch_worker(rank, size, worker_comm, head_comm);
    }
    MPI_Finalize();
    printf("%d: Success!\n", rank);
    return 0;
}
