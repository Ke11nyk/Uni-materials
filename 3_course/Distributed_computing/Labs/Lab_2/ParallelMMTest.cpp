#include "ParallelMM.cpp"

int main(int argc, char* argv[]) {
    double* pAMatrix; // First argument of matrix multiplication
    double* pBMatrix; // Second argument of matrix multiplication
    double* pCMatrix; // Result matrix
    int Size; // Size of matrices
    int BlockSize; // Sizes of matrix blocks
    double *pAblock; // Initial block of matrix A
    double *pBblock; // Initial block of matrix B
    double *pCblock; // Block of result matrix C
    double *pMatrixAblock;
    double Start, Finish, Duration;

    setvbuf(stdout, 0, _IONBF, 0);

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &ProcNum);
    MPI_Comm_rank(MPI_COMM_WORLD, &ProcRank);

    GridSize = sqrt((double)ProcNum);

    // TEST 1-8
    for (int i = 0; i < 8; i++) {
        if (i == 0)
            Size = 10;
        else if (i == 1)
            Size = 100;
        else
            Size = 500 + (i - 2) * 500;

        if (ProcNum != GridSize*GridSize) {
            if (ProcRank == 0) {
                printf ("Number of processes must be a perfect square \n");
            }
        }
        else {
            if (ProcRank == 0)
                printf("Test %d\n", (i + 1));

            // Creating the cartesian grid, row and column communcators
            CreateGridCommunicators();
            
            // Memory allocation and initialization of matrix elements
            ProcessInitializationTest ( pAMatrix, pBMatrix, pCMatrix, pAblock, pBblock,
            pCblock, pMatrixAblock, Size, BlockSize );

            Start = MPI_Wtime();
            DataDistribution(pAMatrix, pBMatrix, pMatrixAblock, pBblock, Size,
            BlockSize);

            // Execution of the Fox method
            ParallelResultCalculation(pAblock, pMatrixAblock, pBblock,
            pCblock, BlockSize);
            //TestBlocks(pCblock, BlockSize, (char*)"Result blocks");

            // Gathering the result matrix
            ResultCollection(pCMatrix, pCblock, Size, BlockSize);
            Finish = MPI_Wtime();
            Duration = Finish-Start;

            TestResult(pAMatrix, pBMatrix, pCMatrix, Size);
            if (ProcRank == 0) {
                printf("Time of execution = %f\n", Duration);
            }

            // Process Termination
            ProcessTermination (pAMatrix, pBMatrix, pCMatrix, pAblock, pBblock,
            pCblock, pMatrixAblock);
        }
    }

    MPI_Finalize();
}