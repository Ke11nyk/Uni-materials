def count_operations_moore_penrose(m, n, iterations):
    operations = 0
    
    # Initial calculation
    operations += m * n * m  # A @ A.T
    operations += m * m  # Adding delta**2 * np.identity(m)
    operations += m * m * m  # Matrix inversion
    operations += n * m * m  # A.T @ inv(...)

    # Per iteration
    operations += iterations * (
        m * n * m +  # A @ A.T
        m * m +  # Adding delta**2 * np.identity(m)
        m * m * m +  # Matrix inversion
        n * m * m +  # A.T @ inv(...)
        n * m +  # Matrix subtraction
        n * m  # Frobenius norm calculation
    )

    return operations

def count_operations_moore_penrose_gradient_descent(m, n, iterations):
    operations = 0
    
    # Operations for a single objective function evaluation
    operations_per_iteration = (
        2 * m * n * n + 11 * n * n * n +  # SVD (approximate)
        m * n +  # A @ A.T
        m * m +  # Adding delta**2 * np.eye(m)
        m * m * m +  # pinv calculation
        m * n * m +  # A @ pinv(...)
        m * n +  # A - ...
        m * n  # Frobenius norm calculation
    )
    
    # Total operations for all iterations
    operations = iterations * operations_per_iteration
    
    # Final pseudo-inverse calculation
    operations += (
        m * n * m +  # A @ A.T
        m * m +  # Adding optimal_delta**2 * np.eye(m)
        2 * m * n * n + 11 * n * n * n +  # SVD for final pinv
        n * m * m  # A.T @ pinv(...)
    )
    
    return operations

def count_operations_greville(m, n):
    operations = n + n  # Initial dot product and division

    for i in range(1, m):
        operations += n * n * i  # A_plus @ current_matrix
        operations += n * n  # subtraction from identity
        operations += n * n  # z @ a
        operations += n  # a.T @ (z @ a)
        operations += n * i * i  # A_plus @ A_plus.T
        operations += n * i  # r @ a
        operations += n  # a.T @ (r @ a)
        operations += 1  # addition
        operations += n * i  # a.T @ A_plus
        operations += n * n  # z @ (a @ (a.T @ A_plus))
        operations += n * i  # subtraction and division
        operations += n * i  # r @ a and division
        operations += n * (i + 1)  # hstack

    return operations

def count_operations_svd(m, n):
    operations = 2 * m * n * n + 11 * n * n * n  # Approximate operations for SVD
    operations += n  # comparisons for rank
    operations += min(m, n)  # divisions for s_plus
    operations += n * min(m, n)  # Vt.T @ np.diag(s_plus)
    operations += n * min(m, n) * m  # (...) @ U.T

    return operations