import numpy as np
import os
from datetime import datetime
from scipy.optimize import minimize_scalar

def setup_logging(method_name):
    log_dir = os.path.join('logs', method_name)
    if not os.path.exists(log_dir):
        os.makedirs(log_dir)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    log_file = os.path.join(log_dir, f'{method_name}_pseudo_inverse_log_{timestamp}.txt')
    
    def log(message):
        with open(log_file, 'a') as f:
            f.write(message + '\n')
    
    return log

def isPsevdoInversed(A, A_psevdo_inverse) -> bool:
    #  A * A+ * A = A;
    if not np.allclose(A @ A_psevdo_inverse @ A, A):
        print('A * A+ * A != A')
        return False
    #  A+ * A * A+ = A+
    elif not np.allclose(A_psevdo_inverse @ A @ A_psevdo_inverse, A_psevdo_inverse):
        print('A+ * A * A+ != A+')
        return False
    #  A * A+ - symmetric matrix m x m
    elif not np.allclose(A @ A_psevdo_inverse, (A @ A_psevdo_inverse).T):
        print('A * A+ - not symmetric matrix m x m')
        return False
    #  A+ * A - symmetric matrix n x n
    elif not np.allclose(A_psevdo_inverse @ A, (A_psevdo_inverse @ A).T):
        print('A+ * A - not symmetric matrix n x n')
        return False
    
    print('Matrix is pseudo-inversed')
    return True


def pseudoInverseMatrix_MoorePenrose(A, eps=1e-6, delta=1000):
    log = setup_logging('moorepenrose')
    m, n = A.shape
    
    A0 = A.T @ np.linalg.inv(A @ A.T + delta**2 * np.identity(m))
    log(f'A0: {A0}')
    
    delta = delta / 2
    log(f'delta: {delta}')

    iterations = 0
    while True:
        A_plus = A.T @ np.linalg.inv(A @ A.T + delta**2 * np.identity(m))
        log(f'A1: {A_plus}')

        if np.linalg.norm(A0 - A_plus, ord=2) < eps:
            log(f'iterations: {iterations}')
            return A_plus, iterations
        
        delta = delta / 2
        log(f'delta: {delta}')

        A0 = A_plus
        iterations += 1

def pseudoInverseMatrix_MoorePenrose_GradientDescent(A, eps=1e-6, delta=1000, max_iterations=1000):
    log = setup_logging('gradient_descent')
    m, n = A.shape

    def pinv(M, rcond=1e-15):
        U, s, Vt = np.linalg.svd(M, full_matrices=False)
        s[s < rcond] = 0
        return Vt.T @ np.diag(np.where(s != 0, 1/s, 0)) @ U.T

    def objective(delta):
        return np.linalg.norm(A - A @ A.T @ pinv(A @ A.T + delta**2 * np.eye(m)) @ A, ord='fro')

    log(f'Starting optimization with eps={eps}, delta={delta}, max_iterations={max_iterations}')

    result = minimize_scalar(
        objective,
        method='brent',
        bracket=(eps, delta),
        options={'maxiter': max_iterations}
    )

    optimal_delta = result.x
    log(f'Optimal delta: {optimal_delta}')
    log(f'Optimization result: {result}')

    A_plus = A.T @ pinv(A @ A.T + optimal_delta**2 * np.eye(m))
    log(f'Computed pseudo-inverse: {A_plus}')

    return A_plus, result.nfev # Return number of function evaluations as iterations

def pseudoInverseMatrix_Greville(A, eps=1e-6, delta=None):
    log = setup_logging('greville')

    m, n = A.shape
    A_plus = np.vstack(A[0] / np.dot(A[0].T, A[0]) if np.dot(A[0].T, A[0]) != 0 else A[0])
    current_matrix = np.array([A[0]])

    for i in range(1, m):
        a = A[i].reshape(-1, 1)
        z = np.identity(current_matrix.shape[1]) - np.dot(A_plus, current_matrix)
        current_matrix = np.vstack([current_matrix, A[i]])
        denum = np.dot(a.T, np.dot(z, a))[0, 0]

        if np.abs(denum) < eps:
            r = np.dot(A_plus, A_plus.T)
            denum = 1 + np.dot(a.T, np.dot(r, a))
            A_plus = np.hstack((A_plus - np.dot(z, np.dot(a, np.dot(a.T, A_plus))) / denum, np.dot(r, a) / denum))
        else:
            A_plus = np.hstack((A_plus - np.dot(z, np.dot(a, np.dot(a.T, A_plus))) / denum, np.dot(z, a) / denum))

        log('-' * 74)
        log(f'inverse_matrix: {A_plus}')

    return A_plus, None

def powerIteration(A, num_iterations=100):
    m, n = A.shape

    if m >= n:
        x = np.random.rand(n)
        for _ in range(num_iterations):
            x = A.T @ (A @ x)
            x /= np.linalg.norm(x)
    else:
        x = np.random.rand(m)
        for _ in range(num_iterations):
            x = A @ (A.T @ x)
            x /= np.linalg.norm(x)
    return x

def customSVD(A, k=None, epsilon=1e-10, max_iterations=1000):
    m, n = A.shape
    k = min(m, n) if k is None else min(k, m, n)
    
    if m >= n:
        U = np.zeros((m, k))
        S = np.zeros(k)
        V = np.zeros((n, k))

        for i in range(k):
            v, iter_ops = powerIteration(A, num_iterations=max_iterations)
            u = A @ v
            sigma = np.linalg.norm(u)
            
            if sigma < epsilon:
                break
            
            u /= sigma
            U[:, i] = u
            S[i] = sigma
            V[:, i] = v
            
            A = A - sigma * np.outer(u, v)
    else:
        V = np.zeros((n, k))
        S = np.zeros(k)
        U = np.zeros((m, k))

        for i in range(k):
            u = powerIteration(A.T, num_iterations=max_iterations)
            v = A.T @ u
            sigma = np.linalg.norm(v)
            
            if sigma < epsilon:
                break
            
            v /= sigma
            V[:, i] = v
            S[i] = sigma
            U[:, i] = u
            
            A = A - sigma * np.outer(u, v)
    
    return U, S, V.T

def pseudoInverseMatrix_SVD(A, eps=1e-6, delta=None):
    log = setup_logging('svd')
    m, n = A.shape
    
    # Perform custom SVD
    U, s, Vt = customSVD(A)
    
    log(f"Singular values: {s}")
    
    # Compute the rank
    r = np.sum(s > eps)
    log(f"Rank of the matrix: {r}")
    
    # Construct Λ+ (Lambda+)
    s_plus = np.zeros_like(s)
    s_plus[:r] = 1 / s[:r]
    
    # Construct A+
    A_plus = (Vt.T @ np.diag(s_plus) @ U.T)
    
    log("Pseudo-inverse matrix:")
    log(str(A_plus))
    
    return A_plus, None