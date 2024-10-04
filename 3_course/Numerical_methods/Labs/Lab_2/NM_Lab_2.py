import numpy as np
import time

BORDER_LENGTH = 100

def create_matrix_and_vector(n):
    A = np.eye(n)
    b = np.zeros(n)

    for k in range(n):
        A[k, k] = 1 + k * 0.1
    for k in range(1, n):
        A[k, 0] = 1
    for k in range(1, n - 1):
        A[k + 1, k] = 1
    A[0, n - 1] = 1

    for k in range(0, n - 1):
        b[k] = np.sin(np.pi / (n - k))

    return A, b

def check_gauss(matr):
    print("\nChecking if this system of linear computations can be solved with Gauss method:")

    start = time.time()
    det = np.linalg.det(matr)
    final = time.time()
    print("Determinant: ", det)
    print("Time: ")
    print(final - start)
    if(det == 0):
        print("Determinant is 0 -> cannot be solved")
        print("-" * BORDER_LENGTH)
        return False
    
    print("All good")
    print("-" * BORDER_LENGTH)

    return True

def gauss(A, b):
    print("GAUSS METHOD")

    if(check_gauss(A) == False):
        return np.zeros(len(b))
    
    matr_len = len(b)
    for i in range(matr_len):
        p = np.identity(matr_len)
        m = np.identity(matr_len)

        print("Current A on step", i, ":\n", A)
        print("\nCurrent b on step", i, ":\n", b)

        max_row = np.argmax(np.abs(A[i:matr_len, i])) + i
        print("\nMax Row Ind:", max_row)

        p[[i, max_row]] = p[[max_row, i]]
        print("\nP matrix:\n", p)

        A = np.matmul(p, A)
        b = np.matmul(p, b.transpose())

        for j in range(matr_len):
            if(j < i):
                m[j, i] = 0
            elif(j == i):
                m[j, i] = 1 / A[j, i]
            else :
                m[j, i] = -A[j, i] / A[i, i]
        
        print("\nM matrix:\n", m)
        print("-" * BORDER_LENGTH)

        A = np.matmul(m, A)
        b = np.matmul(m, b.transpose())
        
    print("Result A:\n", A)
    print("\nResult b:", b)
    print("-" * BORDER_LENGTH)

    x = np.zeros(matr_len)
    for i in range(matr_len - 1, -1, -1):
        x[i] = b[i]
        for j in range(i + 1, matr_len):
            x[i] -= A[i, j] * x[j]
    
    return x

def check_jacobi(matr, eps):
    print("\nChecking if this system of linear computations can be solved with Jacobi method:")

    for i in range(len(matr)):
        if(matr[i,i] == 0):
            print("Unable to solve: one of diagonal elements is 0")
            return False
        
        s = 0
        for j in range(len(matr)):
            if(j != i):
                s += abs(matr[i,j])

        if(s > abs(matr[i,i])):
            print("Unable to solve: sum of absolutes of elements in row is greater than diagonal element in row", i)
            return False
        
    print("All good")
    print("\nChecking the Theorem about convergence condition for any starting approximation for Jacobi method:")

    matr_upper = np.zeros((len(matr), len(matr)))
    matr_lower = np.zeros((len(matr), len(matr)))
    diag = np.zeros((len(matr), len(matr)))

    for i in range(len(matr)):
        diag[i,i] = matr[i,i]
        for j in range(i+1, len(matr)):
            matr_upper[i,j] = matr[i,j]
            matr_lower[j,i] = matr[j,i]
            
    print("\n", matr_upper, "\n\n", matr_lower, "\n\n", diag)

    upper_lower = np.add(matr_upper, matr_lower)
    print("\n", upper_lower)

    d_m = -1 * np.linalg.inv(diag)
    print("\n", d_m)

    B = np.matmul(d_m, upper_lower)
    print("\nB:\n", B)

    u, sig, v = np.linalg.svd(B)
    for i in range(len(B)):
        diag[i][i] = -sig[i]
    
    B = np.add(B, diag)
    det = np.linalg.det(B)

    print("\nSignature numbers of B matrix:", sig)
    print("\nMatrix (A1 + A2 + lD):\n", B, "\n\nAnd its determinant:", det)

    if abs(det) <= eps:
        print("\nConditions are met: Jacobi method converges for all approximations")
    else:
        print("\nConditions are not met: Jacobi method may not converge for all approximations")
    return True
            
    
def jacobi(A, b, eps):
    print("JACOBI METHOD")

    if(check_jacobi(A, eps) == False):
        return np.zeros(len(b))
    
    n = len(b)
    x = np.zeros(len(b))

    print("\nStarting approximation:", x)

    while True:
        x_new = np.zeros(n)

        for i in range(n):
            x_new[i] = (b[i] - np.dot(A[i, :i], x[:i]) - np.dot(A[i, i+1:], x[i+1:])) / A[i, i]

        print("Current approximation with Jacobi:", x_new)
        
        #Using Frobenius norm
        frob = np.linalg.norm(x_new - x)

        print("-" * BORDER_LENGTH)

        if frob < eps:
            print("Frobenius norm:", frob,"is less than", eps, "-> result obtained")
            return x_new
        
        print("Frobenius norm:", frob, "is not less than", eps, "-> continuing iterations")

        x = x_new

def get_error(x_actual, x_approx):
    print("\nCalculating error between actual result (from gauss method) and approximated result (from jacobi method):")
    
    if (np.linalg.norm(x_approx) != 0):
        er = np.linalg.norm(x_approx - x_actual) / np.linalg.norm(x_approx)
    else:
        er = "-"
    print("Error:", er)

def get_cond(matr):
    u, sig, v = np.linalg.svd(matr)
    c = np.linalg.cond(matr)

    print("\nCondition number of a matrix:", c)
    print(max(abs(sig)), " ", min(abs(sig)))

    if(c <= (max(abs(sig)) / min(abs(sig))) + 0.001 and c >= (max(abs(sig)) / min(abs(sig))) - 0.001):
        print("Condition number", c, "is more or equal to", max(abs(sig)) / min(abs(sig)))

    print("-" * BORDER_LENGTH)

print("Enter size of matrix A and vector b:")
n = int(input())
A, b = create_matrix_and_vector(n)

get_cond(A)
gauss_solution = gauss(A, b)
print("Solution (Gauss):", gauss_solution)
print("-" * BORDER_LENGTH)
jacobi_solution = jacobi(A, b, 0.001)
print("-" * BORDER_LENGTH)
print("Solution (Jacobi):", jacobi_solution)
print("-" * BORDER_LENGTH)
print("Solution (Gauss):", gauss_solution)
print("Solution (Jacobi):", jacobi_solution)
get_error(gauss_solution, jacobi_solution)