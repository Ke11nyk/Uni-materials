#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

// Function for the Gauss-Seidel algorithm with time measurement
void ResultCalculation(double* pMatrix, int Size, double &Eps,
int &Iterations) {
    double dm, dmax,temp;
    int i, j; // Loop variables
    Iterations = 0;
    
    do {
        dmax = 0;

        for (i = 1; i < Size - 1; i++)
            for(j = 1; j < Size - 1; j++) {
                temp = pMatrix[Size * i + j];
                pMatrix[Size * i + j] = 0.25 * (pMatrix[Size * i + j + 1] +
                pMatrix[Size * i + j - 1] +
                pMatrix[Size * (i + 1) + j] +
                pMatrix[Size * (i - 1) + j]);
                dm = fabs(pMatrix[Size * i + j] - temp);
                if (dmax < dm) dmax = dm;
            }

        Iterations++;
    } while (dmax > Eps);
}

// Function for computational process termination
void ProcessTermination(double* pMatrix) {
    delete [] pMatrix;
}

// Function for formatted matrix output
void PrintMatrix (double* pMatrix, int RowCount, int ColCount) {
    int i, j; // Loop variables
    for (i=0; i<RowCount; i++) {
        for (j=0; j<ColCount; j++)
            printf("%7.4f ", pMatrix[i*ColCount+j]);
        printf("\n");
    }
}

// Function for simple setting the grid node values
void DummyDataInitialization (double* pMatrix, int Size) {
    int i, j; // Loop variables
    double h = 1.0 / (Size - 1);

    // Setting the grid node values
    for (i=0; i<Size; i++) {
        for (j=0; j<Size; j++)
            if ((i==0) || (i== Size-1) || (j==0) || (j==Size-1))
                pMatrix[i*Size+j] = 100;
            else
                pMatrix[i*Size+j] = 0;
    }
}

// Function for memory allocation and initialization of grid nodes
void ProcessInitialization (double* &pMatrix, int &Size, double &Eps) {
    // Setting the grid size
    do {
        printf("\nEnter the grid size: ");
        scanf("%d", &Size);
        printf("\nChosen grid size = %d", Size);
        if (Size <= 2)
            printf("\nSize of grid must be greater than 2!\n");
    } while (Size <= 2);

    // Setting the required accuracy
    do {
        printf("\nEnter the required accuracy: ");
        scanf("%lf", &Eps);
        printf("\nChosen accuracy = %lf", Eps);
        if (Eps <= 0)
            printf("\nAccuracy must be greater than 0!\n");
    } while (Eps <= 0);

    // Memory allocation
    pMatrix = new double [Size*Size];

    // Setting the grid node values
    DummyDataInitialization(pMatrix, Size);
}

// Function for memory allocation and initialization of grid nodes
void ProcessTestInitialization (double* &pMatrix, int &Size, double &Eps) {
    // Setting the grid size
    if (Size <= 2)
        printf("\nSize of grid must be greater than 2!\n");

    // Setting the required accuracy
    if (Eps <= 0)
        printf("\nAccuracy must be greater than 0!\n");

    // Memory allocation
    pMatrix = new double [Size*Size];

    // Setting the grid node values
    DummyDataInitialization(pMatrix, Size);
}

int main() {
    double* pMatrix; // Matrix of the grid nodes
    int Size; // Matrix size
    double Eps = 10; // Required accuracy
    int Iterations; // Iteration number
    double start, finish, duration; // Time taken for computation

    printf ("Serial Gauss - Seidel algorithm\n");

    // TEST 1-12
    for (int i = 0; i < 12; i++) {
        printf("\nTest %d", (i + 1));
        printf("\n");

        if (i == 0)
            Size = 10;
        else if (i == 1)
            Size = 100;
        else
            Size = (i - 1) * 1000;

        printf("Size: %d", Size);

        // Process initialization
        ProcessTestInitialization(pMatrix, Size, Eps);

        // Gauss-Seidel method
        start = clock();
        ResultCalculation(pMatrix, Size, Eps, Iterations);
        finish = clock();
        duration = (finish-start)/double(CLOCKS_PER_SEC);

        // Printing the result and time
        printf("\nNumber of iterations: %d", Iterations);
        printf("\nComputation time: %f seconds\n", duration);
        
        // Computational process termination
        ProcessTermination(pMatrix);
    }
}