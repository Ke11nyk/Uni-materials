#include "ParallelMV.cpp"

int main(int argc, char* argv[]) {
    double* pMatrix; // First argument - initial matrix
    double* pVector; // Second argument - initial vector
    double* pResult; // Result vector for matrix-vector multiplication
    int Size; // Sizes of initial matrix and vector
    double* pProcRows; // Stripe of the matrix on the current process
    double* pProcResult; // Block of the result vector on the current process
    int RowNum; // Number of rows in the matrix stripe
    double Start, Finish, Duration;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &ProcNum);
    MPI_Comm_rank(MPI_COMM_WORLD, &ProcRank);

    // TEST 1
    if (ProcRank == 0)
        printf ("Parallel matrix-vector multiplication program\nTest 1\n");

    // Memory allocation and data initialization
    Size = 10;
    ProcessInitializationTest(pMatrix, pVector, pResult, pProcRows, pProcResult,
    Size, RowNum);
    Start = MPI_Wtime();

    // Distributing the initial data between the processes
    DataDistribution(pMatrix, pProcRows, pVector, Size, RowNum);

    // Parallel matrix-vector multiplication
    ParallelResultCalculation(pProcRows, pVector, pProcResult, Size, RowNum);

    // Result replication
    ResultReplication(pProcResult, pResult, Size, RowNum);

    Finish = MPI_Wtime();
    Duration = Finish-Start;
    TestResult(pMatrix, pVector, pResult, Size);

    if (ProcRank == 0) {
        printf("\nTime of execution = %f\n", Duration);
    }

    // Process termination
    ProcessTermination(pMatrix, pVector, pResult, pProcRows, pProcResult);
    //MPI_Finalize();

    // TEST 2
    if (ProcRank == 0)
        printf ("\nTest 2\n");

    // Memory allocation and data initialization
    Size = 100;
    ProcessInitializationTest(pMatrix, pVector, pResult, pProcRows, pProcResult,
    Size, RowNum);
    Start = MPI_Wtime();

    // Distributing the initial data between the processes
    DataDistribution(pMatrix, pProcRows, pVector, Size, RowNum);

    // Parallel matrix-vector multiplication
    ParallelResultCalculation(pProcRows, pVector, pProcResult, Size, RowNum);

    // Result replication
    ResultReplication(pProcResult, pResult, Size, RowNum);

    Finish = MPI_Wtime();
    Duration = Finish-Start;
    TestResult(pMatrix, pVector, pResult, Size);

    if (ProcRank == 0) {
        printf("\nTime of execution = %f\n", Duration);
    }

    // Process termination
    ProcessTermination(pMatrix, pVector, pResult, pProcRows, pProcResult);
    //MPI_Finalize();

    // TEST 3-12
    for(int i = 1; i < 11; i++) {
        if (ProcRank == 0)
            printf ("\nTest %d\n", (i + 2));

        // Memory allocation and data initialization
        Size = i * 1000;
        ProcessInitializationTest(pMatrix, pVector, pResult, pProcRows, pProcResult,
        Size, RowNum);
        Start = MPI_Wtime();

        // Distributing the initial data between the processes
        DataDistribution(pMatrix, pProcRows, pVector, Size, RowNum);

        // Parallel matrix-vector multiplication
        ParallelResultCalculation(pProcRows, pVector, pProcResult, Size, RowNum);

        // Result replication
        ResultReplication(pProcResult, pResult, Size, RowNum);

        Finish = MPI_Wtime();
        Duration = Finish-Start;
        TestResult(pMatrix, pVector, pResult, Size);

        if (ProcRank == 0) {
            printf("\nTime of execution = %f\n", Duration);
        }

        // Process termination
        ProcessTermination(pMatrix, pVector, pResult, pProcRows, pProcResult);
        //MPI_Finalize();
        }

    MPI_Finalize();
}