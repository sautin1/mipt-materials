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

    if (size <= 1) {
        fprintf(stderr, "Too few workers\n");
    } else if (rank == 0) {
        launch_master(size-1);
    } else {
        launch_worker(rank);
    }

    MPI_Finalize();

    printf("%d: Success!\n", rank);
    return 0;
}
