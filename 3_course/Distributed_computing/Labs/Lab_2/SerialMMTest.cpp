#include "SerialMM.cpp"

int main() {
    double* pAMatrix; // First argument of matrix multiplication
    double* pBMatrix; // Second argument of matrix multiplication
    double* pCMatrix; // Result matrix
    int Size; // Size of matrices
    time_t start, finish;
    double duration;

    printf("Serial matrix multiplication program\n");

    // TESTS 1-8
    for(int i = 0; i < 8; i++) {
        printf("\nTest %d", (i + 1));

        // Memory allocation and initialization of matrix elements
        for (int i = 0; i < 8; i++) {
            if (i == 0)
                Size = 10;
            else if (i == 1)
                Size = 100;
            else
                Size = 500 + (i - 2) * 500;

            ProcessInitializationTest(pAMatrix, pBMatrix, pCMatrix, Size);
            
            // Matrix multiplication
            start = clock();
            SerialResultCalculation(pAMatrix, pBMatrix, pCMatrix, Size);   finish = clock();
            duration = (finish-start)/double(CLOCKS_PER_SEC);

            // Printing the time spent by matrix multiplication
            printf("Time of execution: %f\n", duration);

            // Computational process termination
            ProcessTermination(pAMatrix, pBMatrix, pCMatrix);
        }
    }
}