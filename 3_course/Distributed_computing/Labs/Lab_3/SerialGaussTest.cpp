#include "./SerialGauss.cpp"

int main() {
    double* pMatrix; // Matrix of the linear system
    double* pVector; // Right parts of the linear system
    double* pResult; // Result vector
    int Size; // Sizes of the initial matrix and the vector
    time_t start, finish;
    double duration;

    printf("Serial Gauss algorithm for solving linear systems\n");

    // TEST 1-8
    for (int i = 0; i < 8; i++) {
        printf("\nTest %d", (i + 1));

        if (i == 0)
                Size = 10;
            else if (i == 1)
                Size = 100;
            else
                Size = 500 + (i - 2) * 500;

        // Memory allocation and definition of objects' elements
        ProcessInitializationTest(pMatrix, pVector, pResult, Size);

        // Execution of the Gauss algorithm
        start = clock();
        SerialResultCalculation(pMatrix, pVector, pResult, Size);
        finish = clock();
        duration = (finish-start)/double(CLOCKS_PER_SEC);

        // Printing the execution time of the Gauss method
        printf("\n Time of execution: %f\n", duration);

        // Computational process termination
        ProcessTermination(pMatrix, pVector, pResult);
    }
}