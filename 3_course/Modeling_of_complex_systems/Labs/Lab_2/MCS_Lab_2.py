import numpy as np
import time
import imageio.v2 as imageio
import matplotlib.pyplot as plt
import PseudoInverseMatrixMethods as pimm
import OperationsCounter as oc
import json
import psutil
import os

def readImage(filename):
    X = imageio.imread(filename)
    return np.array(X)

def calculateOperator(X, Y, inversion_function, operations_calculation, V=None, eps=1e-6, delta=10):
    if V is None:
        V = np.zeros((Y.shape[0], X.shape[0]))

    start_time = time.time()
    start_memory = psutil.Process(os.getpid()).memory_info().rss / 1024 / 1024  # Memory in MB

    X_pinv, iteration_count = inversion_function(X, eps=eps, delta=delta)

    if iteration_count is not None:
        operation_count = operations_calculation(X.shape[0], X.shape[1], iteration_count)
    else:
        operation_count = operations_calculation(X.shape[0], X.shape[1])

    end_time = time.time()
    end_memory = psutil.Process(os.getpid()).memory_info().rss / 1024 / 1024  # Memory in MB

    inversion_time = end_time - start_time
    memory_used = end_memory - start_memory
    
    YX_pinv = Y @ X_pinv
    Z = np.eye(X.shape[0]) - X @ X_pinv
    
    VZ = V @ Z.T
    A = YX_pinv + VZ

    return A, inversion_time, memory_used, operation_count

def applyOperator(X_, A_):
    return A_ @ X_

def saveImage(X_, filename):
    results_dir = 'results'
    if not os.path.exists(results_dir):
        os.makedirs(results_dir)
    
    filepath = os.path.join(results_dir, filename)
    plt.figure(figsize=(10, 10))
    plt.imshow(X_, cmap='gray')
    plt.axis('off')
    plt.savefig(filepath, bbox_inches='tight', pad_inches=0)
    plt.close()

def calculateError(Y, Y_expected):
    error_norm = np.linalg.norm(Y - Y_expected, ord=1)
    mse = np.mean((Y - Y_expected)**2)
    rmse = np.sqrt(mse)
    return error_norm, mse, rmse

def numpy_to_python(obj):
    if isinstance(obj, np.integer):
        return int(obj)
    elif isinstance(obj, np.floating):
        return float(obj)
    elif isinstance(obj, np.ndarray):
        return obj.tolist()
    else:
        return obj

def main():
    X = readImage('x1.bmp')
    Y = readImage('y4.bmp')

    m = X.shape[1]
    X = np.vstack((X, np.ones((1, m))))

    saveImage(Y, 'original_Y.png')

    results = {}

    # Moore-Penrose method (original)
    A_MP, time_MP, memory_MP, ops_MP = calculateOperator(X, Y, pimm.pseudoInverseMatrix_MoorePenrose, oc.count_operations_moore_penrose, eps=1e-60, delta=1000)
    Y_MP = applyOperator(X, A_MP)
    error_norm_MP, mse_MP, rmse_MP = calculateError(Y, Y_MP)

    # Moore-Penrose method (gradient descent)
    A_MP_GD, time_MP_GD, memory_MP_GD, ops_MP_GD = calculateOperator(X, Y, pimm.pseudoInverseMatrix_MoorePenrose_GradientDescent, oc.count_operations_moore_penrose_gradient_descent, eps=1e-60, delta=1000)
    Y_MP_GD = applyOperator(X, A_MP_GD)
    error_norm_MP_GD, mse_MP_GD, rmse_MP_GD = calculateError(Y, Y_MP_GD)

    # Compare and choose the better Moore-Penrose result
    if rmse_MP < rmse_MP_GD:
        saveImage(Y_MP, 'moore_penrose_result.png')
        best_MP = {
            'method': 'Moore-Penrose',
            'time': time_MP,
            'memory': memory_MP,
            'operations': ops_MP,
            'error_norm': error_norm_MP,
            'mse': mse_MP,
            'rmse': rmse_MP
        }
    else:
        saveImage(Y_MP_GD, 'moore_penrose_result.png')
        best_MP = {
            'method': 'Gradient Descent',
            'time': time_MP_GD,
            'memory': memory_MP_GD,
            'operations': ops_MP_GD,
            'error_norm': error_norm_MP_GD,
            'mse': mse_MP_GD,
            'rmse': rmse_MP_GD
        }

    results['Moore-Penrose'] = {
        'time': time_MP,
        'memory': memory_MP,
        'operations': ops_MP,
        'error_norm': error_norm_MP,
        'mse': mse_MP,
        'rmse': rmse_MP
    }

    results['Gradient Descent'] = {
        'time': time_MP_GD,
        'memory': memory_MP_GD,
        'operations': ops_MP_GD,
        'error_norm': error_norm_MP_GD,
        'mse': mse_MP_GD,
        'rmse': rmse_MP_GD
    }

    results['Best Moore-Penrose'] = best_MP

    # Greville method
    A_G, time_G, memory_G, ops_G = calculateOperator(X, Y, pimm.pseudoInverseMatrix_Greville, oc.count_operations_greville, eps=1e-60)
    Y_G = applyOperator(X, A_G)
    saveImage(Y_G, 'greville_result.png')
    error_norm_G, mse_G, rmse_G = calculateError(Y, Y_G)
    results['Greville'] = {
        'time': time_G,
        'memory': memory_G,
        'operations': ops_G,
        'error_norm': error_norm_G,
        'mse': mse_G,
        'rmse': rmse_G
    }

    # SVD method
    A_SVD, time_SVD, memory_SVD, ops_SVD = calculateOperator(X, Y, pimm.pseudoInverseMatrix_SVD, oc.count_operations_svd, eps=1e-60)
    Y_SVD = applyOperator(X, A_SVD)
    saveImage(Y_SVD, 'svd_result.png')
    error_norm_SVD, mse_SVD, rmse_SVD = calculateError(Y, Y_SVD)
    results['SVD'] = {
        'time': time_SVD,
        'memory': memory_SVD,
        'operations': ops_SVD,
        'error_norm': error_norm_SVD,
        'mse': mse_SVD,
        'rmse': rmse_SVD
    }

    # Convert NumPy types to Python native types
    results = {k: {kk: numpy_to_python(vv) for kk, vv in v.items()} for k, v in results.items()}

    # Save results to a JSON file
    with open('results.json', 'w') as f:
        json.dump(results, f, indent=2)

if __name__ == "__main__":
    main()