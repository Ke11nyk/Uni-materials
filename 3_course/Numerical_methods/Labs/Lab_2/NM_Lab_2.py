import numpy as np

def create_matrix_and_vector(n):
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

def gauss(A, b):
    steps = []
    A = A.copy()
    b = b.copy()
    n = len(b)
    
    for i in range(n):
        max_row = i + np.argmax(np.abs(A[i:, i]))
        A[[i, max_row]] = A[[max_row, i]]
        b[[i, max_row]] = b[[max_row, i]]
        
        for j in range(i+1, n):
            factor = A[j, i] / A[i, i]
            A[j, i:] -= factor * A[i, i:]
            b[j] -= factor * b[i]
        
        steps.append({
            'step': i+1,
            'A': A.copy(),
            'b': b.copy()
        })
    
    x = np.zeros(n)
    for i in range(n-1, -1, -1):
        x[i] = (b[i] - np.dot(A[i, i+1:], x[i+1:])) / A[i, i]
    
    steps.append({
        'step': 'final',
        'x': x
    })
    
    return x, steps

def jacobi(A, b, eps, max_iterations=1000):
    steps = []
    n = len(b)
    x = np.zeros(n)
    D = np.diag(A)
    R = A - np.diagflat(D)
    
    for iteration in range(max_iterations):
        x_new = (b - np.dot(R, x)) / D
        steps.append({
            'iteration': iteration+1,
            'x': x_new.copy()
        })
        
        if np.linalg.norm(x_new - x) < eps:
            return x_new, steps
        
        x = x_new
    
    return x, steps

def get_error(x_actual, x_approx):
    return np.linalg.norm(x_approx - x_actual) / np.linalg.norm(x_actual)

def solve_linear_system(n, eps=1e-6):
    # Create matrix A and vector b
    A, b = create_matrix_and_vector(n)
    
    # Solve using Gauss method
    gauss_solution, gauss_steps = gauss(A, b)
    
    # Solve using Jacobi method
    jacobi_solution, jacobi_steps = jacobi(A, b, eps)
    
    # Calculate error
    error = get_error(gauss_solution, jacobi_solution)
    
    return {
        'matrix_A': A,
        'vector_b': b,
        'gauss_solution': gauss_solution,
        'gauss_steps': gauss_steps,
        'jacobi_solution': jacobi_solution,
        'jacobi_steps': jacobi_steps,
        'error': error
    }

# Example usage
n = 11  # Small size for demonstration
result = solve_linear_system(n)

print("Matrix A:")
print(result['matrix_A'])
print("\nVector b:")
print(result['vector_b'])
print("\nGauss solution:")
print(result['gauss_solution'])
print("\nJacobi solution:")
print(result['jacobi_solution'])
print("\nError between Gauss and Jacobi solutions:")
print(result['error'])

print("\nGauss steps:")
for step in result['gauss_steps']:
    print(f"Step {step['step']}:")
    if 'A' in step:
        print("A:")
        print(step['A'])
        print("b:")
        print(step['b'])
    else:
        print("Final solution:")
        print(step['x'])

print("\nJacobi steps:")
for step in result['jacobi_steps'][:5]:  # Print first 5 steps
    print(f"Iteration {step['iteration']}:")
    print(step['x'])
if len(result['jacobi_steps']) > 5:
    print("...")  # Indicate that there are more steps