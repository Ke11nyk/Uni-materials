int Min(int A, int B);
void ProcessInitialization(int *&pMatrix, int *&pProcRows, int& Size,
int& RowNum);
void ProcessInitializationTest(int *&pMatrix, int *&pProcRows, int& Size,
int& RowNum);
void ProcessTermination(int *pMatrix, int *pProcRows);
void DummyDataInitialization(int *pMatrix, int Size);
void DataDistribution(int *pMatrix, int *pProcRows, int Size, int RowNum);
void ResultCollection(int *pMatrix, int *pProcRows, int Size, int RowNum);
void ParallelFloyd(int *pProcRows, int Size, int RowNum);
void RowDistribution(int *pProcRows, int Size, int RowNum, int k, int *pRow);
void ParallelPrintMatrix(int *pProcRows, int Size, int RowNum);
void TestDistribution(int *pMatrix, int *pProcRows, int Size, int RowNum);
void TestResult(int *pMatrix, int *pSerialMatrix, int Size);