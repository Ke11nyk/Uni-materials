import numpy as np
import time
import PseudoInverseMatrixMethods as pimm
import OperationsCounter as oc
import json
import psutil
import os
import cv2

#reading input images X and Y
def readImage():
    #read the image & convert it to float32 for multiplication
    x_img = cv2.imread("x1.bmp", cv2.IMREAD_GRAYSCALE)
    y_img = cv2.imread("y4.bmp", cv2.IMREAD_GRAYSCALE)
    
    #display the image
    cv2.imshow("Image x", x_img)
    cv2.imshow("Image y", y_img)
    #wait for the user to press a key
    cv2.waitKey(0)
    #close all windows
    cv2.destroyAllWindows()
        
    x = x_img.astype(float)
    y = y_img.astype(float)

    print(f"x size: {x.shape} , y size: {y.shape}")
    return x, y

def saveImage(matrix, filename="output.png"):
    # Create results directory if it doesn't exist
    results_dir = "results"
    if not os.path.exists(results_dir):
        os.makedirs(results_dir)
    
    # Ensure the filename ends with .png
    if not filename.endswith('.png'):
        filename = filename + '.png'
    
    # Create full path for saving
    save_path = os.path.join(results_dir, filename)
    
    # Ensure the matrix values are in valid range for uint8
    # First normalize to 0-1 range
    normalized = (matrix - matrix.min()) / (matrix.max() - matrix.min())
    # Then scale to 0-255 range
    img = (normalized * 255).astype('uint8')
    
    # Save the image in PNG format to results folder
    cv2.imwrite(save_path, img)
    
    # Display the image
    cv2.imshow("Converted Image", img)
    cv2.waitKey(0)
    cv2.destroyAllWindows()
    
    return img

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

def calculateError(Y, Y_expected):
    error_norm = np.linalg.norm(Y - Y_expected, ord=1)
    mse = np.mean((Y - Y_expected)**2)
    rmse = np.sqrt(mse)
    return error_norm, mse, rmse

def numpyToPython(obj):
    if isinstance(obj, np.integer):
        return int(obj)
    elif isinstance(obj, np.floating):
        return float(obj)
    elif isinstance(obj, np.ndarray):
        return obj.tolist()
    else:
        return obj

def main():
    X, Y = readImage()

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
    results = {k: {kk: numpyToPython(vv) for kk, vv in v.items()} for k, v in results.items()}

    # Save results to a JSON file
    with open('results.json', 'w') as f:
        json.dump(results, f, indent=2)

if __name__ == "__main__":
    main()