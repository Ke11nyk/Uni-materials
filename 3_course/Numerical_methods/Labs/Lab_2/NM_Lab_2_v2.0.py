import numpy as np

# Create matrix A and vector b
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

# Find condition number
def condition_number(A):
    return np.linalg.cond(A)

# Gaussian elimination with selection of the main one by column
def gaussian_elimination(A, b=None, find_inverse=False, find_determinant=False, log_file='gaussian_log.txt'):
    n = len(A)
    if b is not None:
        Ab = np.column_stack((A, b))
    elif find_inverse:
        Ab = np.column_stack((A, np.eye(n)))
    else:
        Ab = np.array(A, dtype=float)
    
    det = 1.0
    
    with open(log_file, 'w') as log:
        log.write("Starting Gaussian Elimination\n")
        log.write(f"Initial augmented matrix:\n{Ab}\n\n")

        for k in range(n):
            # Partial pivoting
            max_element = abs(Ab[k:, k]).argmax() + k
            if max_element != k:
                Ab[[k, max_element]] = Ab[[max_element, k]]
                det *= -1
                log.write(f"Row swap: {k+1} <-> {max_element+1}\n")
                log.write(f"Matrix after row swap:\n{Ab}\n\n")

            pivot = Ab[k, k]
            if abs(pivot) < 1e-10:
                log.write("Matrix is singular or nearly singular. Cannot apply Gaussian elimination.\n")
                return None

            det *= pivot

            for i in range(k + 1, n):
                factor = Ab[i, k] / pivot
                Ab[i, k:] -= factor * Ab[k, k:]
                log.write(f"Eliminate variable x{k+1} from equation {i+1}\n")
                log.write(f"Matrix after elimination:\n{Ab}\n\n")

        log.write("Gaussian Elimination completed. Starting back-substitution.\n")

        # Back-substitution
        if b is not None or find_inverse:
            x = np.zeros((n, Ab.shape[1] - n))
            for i in range(n - 1, -1, -1):
                x[i] = (Ab[i, n:] - np.dot(Ab[i, i+1:n], x[i+1:])) / Ab[i, i]
                log.write(f"Back-substitution step {n-i}: x{i+1} = {x[i]}\n")

        log.write("Process completed.\n")

    if find_determinant:
        return det
    elif find_inverse:
        return x
    elif b is not None:
        return x.flatten()
    else:
        return Ab[:, :n]

# Conditions of jacobi method
def is_diagonally_dominant(A):
    return np.all(np.abs(np.diag(A)) > np.sum(np.abs(A), axis=1) - np.abs(np.diag(A)))

def spectral_radius(A):
    D = np.diag(np.diag(A))
    L_plus_U = A - D
    B = -np.linalg.inv(D) @ L_plus_U
    return max(abs(np.linalg.eigvals(B)))

def is_jacobi_applicable(A):
    return is_diagonally_dominant(A) or spectral_radius(A) < 1

# Jacobi method
def jacobi_method(A, b, x0=None, tol=1e-10, max_iter=1000, log_file='jacobi_log.txt'):
    n = len(A)
    if x0 is None:
        x0 = np.zeros(n)
    
    x = x0.copy()
    
    with open(log_file, 'w') as log:
        log.write("Starting Jacobi Method\n")
        log.write(f"Initial guess: {x}\n\n")

        for iteration in range(max_iter):
            x_new = np.zeros(n)
            for i in range(n):
                s = sum(A[i, j] * x[j] for j in range(n) if j != i)
                x_new[i] = (b[i] - s) / A[i, i]
            
            log.write(f"Iteration {iteration + 1}:\n")
            log.write(f"x = {x_new}\n")
            
            if np.linalg.norm(x_new - x, np.inf) < tol:
                log.write(f"\nConverged after {iteration + 1} iterations.\n")
                return x_new
            
            x = x_new
            log.write(f"Error: {np.linalg.norm(x_new - x, np.inf)}\n\n")
    
        log.write("Jacobi method did not converge within the maximum number of iterations.\n")
    return x

# Calculate error between two results
def calculate_error(x1, x2):
    """
    Calculate the absolute and relative errors between two solutions.
    """
    absolute_error = np.linalg.norm(x1 - x2)
    relative_error = absolute_error / np.linalg.norm(x1)
    return absolute_error, relative_error

def main():
    # Example usage
    A = np.array([[10.0, 2.0, -1.0, -3.0, 4.0], [2.0, 12.0, 3.0, 1.0, -5.0], [-1.0, 3.0, 14.0, -4.0, 6.0], [-3.0, 1.0, -4.0, 15.0, 2.0], [4.0, -5.0, 6.0, 2.0, 17.0]])
    b = np.array([20.0, 30.0, 40.0, 50.0, 60.0])

    print("Condition number of A:", condition_number(A))

    # Solve Ax = b using Gaussian elimination
    det_A = gaussian_elimination(A, find_determinant=True)
    if det_A != 0:
        x_gaussian = gaussian_elimination(A, b)
        print("\nSolution using Gaussian elimination:")
        print(x_gaussian)
        
        # Find inverse of A
        A_inv = gaussian_elimination(A, find_inverse=True)
        print("\nInverse of A:")
        print(A_inv)
        
        print("\nDeterminant of A (using Gaussian elimination):")
        print(det_A)
    else:
        print("Gaussian elimination is not applicable. The matrix is singular.")

    # Solve Ax = b using Jacobi method
    if is_jacobi_applicable(A):
        print("\nJacobi method is applicable.")
        
        x_jacobi = jacobi_method(A, b)
        print("\nSolution using Jacobi method:")
        print(x_jacobi)
    else:
        print("Jacobi method may not converge for this system.")

    print("\nCheck the 'gaussian_log.txt' and 'jacobi_log.txt' files for step-by-step logs.")

if __name__ == "__main__":
    main()