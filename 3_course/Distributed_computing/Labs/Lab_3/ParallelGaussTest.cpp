#include "ParallelGauss.cpp"

int main(int argc, char* argv[]) {
    double* pMatrix; // Matrix of the linear system
    double* pVector; // Right parts of the linear system
    double* pResult; // Result vector
    double *pProcRows; // Rows of the matrix A
    double *pProcVector; // Elements of the vector b
    double *pProcResult; // Elements of the vector x
    int Size; // Sizes of the matrix and the vectors
    int RowNum; // Number of the matrix rows
    double start, finish, duration;

    setvbuf(stdout, 0, _IONBF, 0);
    MPI_Init ( &argc, &argv );
    MPI_Comm_rank ( MPI_COMM_WORLD, &ProcRank );
    MPI_Comm_size ( MPI_COMM_WORLD, &ProcNum );

    // if (ProcRank == 0)
    //     printf("Parallel Gauss algorithm for solving linear systems\n");

    // TEST 1-8
    for (int i = 0; i < 8; i++) {
        //printf("\nTest %d\n", (i + 1));

        if (i == 0)
                Size = 10;
            else if (i == 1)
                Size = 100;
            else
                Size = 500 + (i - 2) * 500;

        // Memory allocation and data initialization
        ProcessInitializationTest(pMatrix, pVector, pResult,
        pProcRows, pProcVector, pProcResult, Size, RowNum);

        // The execution of the parallel Gauss algorithm
        start = MPI_Wtime();

        DataDistribution(pMatrix, pProcRows, pVector, pProcVector, Size, RowNum);
        ParallelResultCalculation(pProcRows, pProcVector, pProcResult,
        Size, RowNum);
        // TestDistribution(pMatrix, pVector, pProcRows, pProcVector, Size, RowNum);
        ResultCollection(pProcResult, pResult);

        finish = MPI_Wtime();

        duration = finish-start;

        TestResult(pMatrix, pVector, pResult, Size);

        // Printing the time spent by the Gauss algorithm
        if (ProcRank == 0)
            printf("\n Time of execution: %f\n\n", duration);
            
        // Computational process termination
        ProcessTermination(pMatrix, pVector, pResult, pProcRows, pProcVector,
        pProcResult);
    }

    MPI_Finalize();
}