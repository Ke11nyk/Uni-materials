#include "SerialMV.cpp"

int main() {
    double* pMatrix;        // First argument - initial matrix
    double* pVector;        // Second argument - initial vector
    double* pResult;        // Result vector for matrix-vector multiplication
    int Size;               // Sizes of initial matrix and vector
    time_t start, finish;
    double duration;

    // TEST 1
    printf("Serial matrix-vector multiplication program\nTest 1");

    // Memory allocation and data initialization
    Size = 10;
    ProcessInitializationTest(pMatrix, pVector, pResult, Size);

    // Matrix-vector multiplication
    start = clock();
    ResultCalculation(pMatrix, pVector, pResult, Size);
    finish = clock();
    duration = (finish-start)/double(CLOCKS_PER_SEC);

    // Printing the time spent by matrix-vector multiplication
    printf("Time of execution: %f\n", duration);

    // Computational process termination
    ProcessTermination(pMatrix, pVector, pResult);

    // TEST 2
    printf("\nTest 2");

    // Memory allocation and data initialization
    Size = 100;
    ProcessInitializationTest(pMatrix, pVector, pResult, Size);

    // Matrix-vector multiplication
    start = clock();
    ResultCalculation(pMatrix, pVector, pResult, Size);
    finish = clock();
    duration = (finish-start)/double(CLOCKS_PER_SEC);

    // Printing the time spent by matrix-vector multiplication
    printf("Time of execution: %f\n", duration);

    // Computational process termination
    ProcessTermination(pMatrix, pVector, pResult);

    // TEST 3-12
    for(int i = 1; i < 11; i++) {
        printf("\nTest %d", (i + 2));

        // Memory allocation and data initialization
        Size = i * 1000;
        ProcessInitializationTest(pMatrix, pVector, pResult, Size);

        // Matrix-vector multiplication
        start = clock();
        ResultCalculation(pMatrix, pVector, pResult, Size);
        finish = clock();
        duration = (finish-start)/double(CLOCKS_PER_SEC);

        // Printing the time spent by matrix-vector multiplication
        printf("Time of execution: %f\n", duration);

        // Computational process termination
        ProcessTermination(pMatrix, pVector, pResult);
    }
}