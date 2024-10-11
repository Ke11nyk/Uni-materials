import numpy as np
import time

BORDER_LENGTH = 100
MAX_STEPS = 5

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

def test_matrix_and_vector():
    A = np.array([[10.0, 2.0, -1.0, -3.0, 4.0], [2.0, 12.0, 3.0, 1.0, -5.0], [-1.0, 3.0, 14.0, -4.0, 6.0], [-3.0, 1.0, -4.0, 15.0, 2.0], [4.0, -5.0, 6.0, 2.0, 17.0]])
    b = np.array([20.0, 30.0, 40.0, 50.0, 60.0])

    return A, b

def gauss_elimination(A, b=None, calculate_determinant=False):
    n = len(A)
    if b is None:
        b = np.zeros(n)
    det = 1.0
    
    for i in range(n):
        max_element = abs(A[i][i])
        max_row = i
        for k in range(i + 1, n):
            if abs(A[k][i]) > max_element:
                max_element = abs(A[k][i])
                max_row = k

        if max_row != i:
            A[i], A[max_row] = A[max_row].copy(), A[i].copy()
            b[i], b[max_row] = b[max_row], b[i]
            det *= -1

        pivot = A[i][i]
        if pivot == 0:
            if calculate_determinant:
                return 0
            else:
                raise ValueError("Matrix is singular")

        det *= pivot

        for k in range(i + 1, n):
            factor = A[k][i] / pivot
            for j in range(i, n):
                A[k][j] -= factor * A[i][j]
            b[k] -= factor * b[i]

    if calculate_determinant:
        return det
    else:
        return A, b

def gauss_determinant(A):
    return gauss_elimination(A.copy(), calculate_determinant=True)

def check_gauss(matr):
    print("\nChecking if this system of linear computations can be solved with Gauss method:")

    det = gauss_determinant(matr)
    print("Determinant: ", det)
    if(det == 0):
        print("Determinant is 0 -> cannot be solved")
        print("-" * BORDER_LENGTH)
        return False
    
    print("All good")
    print("-" * BORDER_LENGTH)

    return True

def gauss(A, b):
    print("GAUSS METHOD")

    if not check_gauss(A):
        return np.zeros(len(b))
    
    A_copy, b_copy = A.copy(), b.copy()
    n = len(b)
    steps_to_show = min(MAX_STEPS - 1, n - 1)

    for i in range(n):
        if i < steps_to_show or i == n - 1:
            print(f"\nStep {i + 1}:")
            print("Current A:\n", A_copy)
            print("Current b:\n", b_copy)
        elif i == steps_to_show:
            print("\n... (intermediate steps omitted) ...")

        A_copy, b_copy = gauss_elimination(A_copy, b_copy)

    print("-" * BORDER_LENGTH)

    x = np.zeros(n)
    for i in range(n - 1, -1, -1):
        x[i] = b_copy[i]
        for j in range(i + 1, n):
            x[i] -= A_copy[i][j] * x[j]
        x[i] /= A_copy[i][i]
    
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

    upper_lower = np.add(matr_upper, matr_lower)

    d_m = -1 * np.linalg.inv(diag)

    B = np.matmul(d_m, upper_lower)

    u, sig, v = np.linalg.svd(B)
    for i in range(len(B)):
        diag[i][i] = -sig[i]
    
    B = np.add(B, diag)
    det = gauss_determinant(B)

    print("\nSignature numbers of B matrix:", sig)
    print("\nMatrix (A1 + A2 + lD):\n", B, "\n\nAnd its determinant:", det)

    if abs(det) <= eps:
        print("\nConditions are met: Jacobi method converges for all approximations")
    else:
        print("\nConditions are not met: Jacobi method may not converge for all approximations")
    return True
            
def euclid_norm(vector):
    return np.sqrt(np.sum(np.square(vector)))

def jacobi(A, b, eps):
    print("JACOBI METHOD")

    if(check_jacobi(A, eps) == False):
        return np.zeros(len(b))
    
    n = len(b)
    x = np.zeros(len(b))

    print("\nStarting approximation:", x)

    steps_taken = 0
    while True:
        x_new = np.zeros(n)

        for i in range(n):
            x_new[i] = (b[i] - np.dot(A[i, :i], x[:i]) - np.dot(A[i, i+1:], x[i+1:])) / A[i, i]

        if steps_taken < MAX_STEPS - 1:
            print(f"\nStep {steps_taken + 1}:")
            print("Current approximation:", x_new)
        elif steps_taken == MAX_STEPS - 1:
            print("\n... (intermediate steps omitted) ...")

        #euc = np.linalg.norm(x_new - x)

        euc = euclid_norm(x_new - x)

        steps_taken += 1

        if euc < eps:
            print(f"\nFinal Step {steps_taken}:")
            print("Final approximation:", x_new)
            print("Euclid norm:", euc,"is less than", eps, "-> result obtained")
            return x_new
        
        x = x_new

def get_error(x_actual, x_approx):
    print("\nCalculating error between actual result (from gauss method) and approximated result (from jacobi method):")
    
    if (np.linalg.norm(x_approx) != 0):
        er = np.linalg.norm(x_approx - x_actual) / np.linalg.norm(x_approx)
    else:
        er = "-"
    print("Error:", er)

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

def inverse_matrix(A):
    n = len(A)
    inv = np.eye(n)
    
    for i in range(n):
        if A[i][i] == 0:
            raise ValueError("Matrix is not invertible")
        
        for j in range(n):
            if i != j:
                ratio = A[j][i] / A[i][i]
                for k in range(n):
                    A[j][k] = A[j][k] - ratio * A[i][k]
                    inv[j][k] = inv[j][k] - ratio * inv[i][k]
    
    for i in range(n):
        divisor = A[i][i]
        for j in range(n):
            A[i][j] = A[i][j] / divisor
            inv[i][j] = inv[i][j] / divisor
    
    return inv

# print("Enter size of matrix A and vector b:")
# n = int(input())
# A, b = create_matrix_and_vector(n)

A, b = test_matrix_and_vector()

get_cond(A)

# Compare determinant calculation methods
start_time = time.time()
np_det = np.linalg.det(A)
np_det_time = time.time() - start_time

start_time = time.time()
gauss_det = gauss_determinant(A.copy())
gauss_det_time = time.time() - start_time

print(f"NumPy determinant: {np_det}, Time: {np_det_time:.6f} seconds")
print(f"Gauss determinant: {gauss_det}, Time: {gauss_det_time:.6f} seconds")
print("-" * BORDER_LENGTH)

# Calculate inverse matrix
start_time = time.time()
inv_A = inverse_matrix(A.copy())
inv_time = time.time() - start_time
print(f"Inverse matrix calculation time: {inv_time:.6f} seconds")
print("Inverse matrix:")
print(inv_A)
print("-" * BORDER_LENGTH)

# Gauss method
start_time = time.time()
gauss_solution = gauss(A.copy(), b.copy())
gauss_time = time.time() - start_time
print(f"Gauss method execution time: {gauss_time:.6f} seconds")
print("Solution (Gauss):", gauss_solution)
print("-" * BORDER_LENGTH)

# Jacobi method
start_time = time.time()
jacobi_solution = jacobi(A.copy(), b.copy(), 0.001)
jacobi_time = time.time() - start_time
print(f"Jacobi method execution time: {jacobi_time:.6f} seconds")
print("Solution (Jacobi):", jacobi_solution)
print("-" * BORDER_LENGTH)

print("Solution (Gauss):", gauss_solution)
print("Solution (Jacobi):", jacobi_solution)
get_error(gauss_solution, jacobi_solution)