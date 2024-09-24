import numpy as np

def create_matrix_and_vector(n, k):
    A = np.eye(n)
    for k in range(n):
        A[k, k] = 1 + k * 0.1
    for k in range(1, n):
        A[k, 0] = 1
    for k in range(1, n - 1):
        A[k + 1, k] = 1
    A[0, n - 1] = 1
    b = np.sin(np.arange(1, n+1) * np.pi / n)
    return A, b

def swap_rows(matrix, row1, row2):
    matrix[[row1, row2]] = matrix[[row2, row1]]
    return matrix

def to_non_zero_diagonal(matrix, eps):
    n = matrix.shape[0]
    for i in range(n):
        if np.abs(matrix[i, i]) < eps:
            for j in range(i+1, n):
                if np.abs(matrix[j, i]) > eps:
                    matrix = swap_rows(matrix, i, j)
                    break
            else:
                raise ValueError("Unable to create a matrix with non-zero diagonal")
    return matrix

def is_diagonally_dominant(matrix, eps):
    n = matrix.shape[0]
    has_strict = False
    for i in range(n):
        diag = np.abs(matrix[i, i])
        row_sum = np.sum(np.abs(matrix[i, :])) - diag
        if diag < row_sum:
            return False
        if diag - eps > row_sum:
            has_strict = True
    return has_strict

def gauss_seidel(matrix, b, x0, eps, max_iterations=1000):
    matrix = to_non_zero_diagonal(matrix, eps)
    if not is_diagonally_dominant(matrix, eps):
        print("Warning: Matrix is not diagonally dominant. Convergence may be slow or fail.")
    
    n = matrix.shape[0]
    x = x0.copy()
    for iteration in range(max_iterations):
        x_old = x.copy()
        for i in range(n):
            s1 = np.dot(matrix[i, :i], x[:i])
            s2 = np.dot(matrix[i, i+1:], x_old[i+1:])
            x[i] = (b[i] - s1 - s2) / matrix[i, i]
        if np.linalg.norm(x - x_old) < eps:
            print(f"Gauss-Seidel converged in {iteration + 1} iterations")
            return x
    print("Gauss-Seidel did not converge within the maximum number of iterations")
    return x

def jacobi(matrix, b, x0, eps, max_iterations=1000):
    matrix = to_non_zero_diagonal(matrix, eps)
    if not is_diagonally_dominant(matrix, eps):
        print("Warning: Matrix is not diagonally dominant. Convergence may be slow or fail.")
    
    n = matrix.shape[0]
    x = x0.copy()
    for iteration in range(max_iterations):
        x_new = np.zeros(n)
        for i in range(n):
            s = np.dot(matrix[i, :], x) - matrix[i, i] * x[i]
            x_new[i] = (b[i] - s) / matrix[i, i]
        if np.linalg.norm(x_new - x) < eps:
            print(f"Jacobi converged in {iteration + 1} iterations")
            return x_new
        x = x_new
    print("Jacobi did not converge within the maximum number of iterations")
    return x

# Set up the problem
n = 11
k = 0.1
eps = 1e-6
A, b = create_matrix_and_vector(n, k)
x0 = np.zeros(n)

# Solve using Gauss-Seidel method
x_gauss_seidel = gauss_seidel(A, b, x0, eps)

# Solve using Jacobi method
x_jacobi = jacobi(A, b, x0, eps)

print('-' * 85)

# Print results
print("Gauss-Seidel solution:", x_gauss_seidel)
print("\nJacobi solution:", x_jacobi)

print('-' * 85)

# Verify the solutions
print("Gauss-Seidel error:", np.linalg.norm(A @ x_gauss_seidel - b))
print("Jacobi error:", np.linalg.norm(A @ x_jacobi - b))