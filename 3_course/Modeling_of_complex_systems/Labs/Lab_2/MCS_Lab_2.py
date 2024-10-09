import numpy as np
import imageio.v2 as imageio
import matplotlib.pyplot as plt
import PseudoInverseMatrixMethods as pimm

def read_image(filename):
    # Read the .bmp input and output files as a numpy 2D matrix
    X = imageio.imread(filename)
    # Convert the image to a NumPy array
    return np.array(X)

def CalculateOperator(X, Y, inversion_function, V=None, eps=1e-2, delta=10):
    if V is None:
        V = np.zeros((Y.shape[0], X.shape[0]))

    X_pinv = inversion_function(X, eps=eps, delta=delta)
    # assert pimm.isPsevdoInversed(X, X_pinv)
        
    YX_pinv = Y @ X_pinv
    Z = np.eye(X.shape[0]) - X @ X_pinv
    
    VZ = V @ Z.T
    A = YX_pinv + VZ
    return A


def ApplyOperator(X_, A_):
    return A_ @ X_

def ShowImage(X_):
    plt.imshow(X_, cmap='gray')
    plt.show()


def CalculateError1(Y, Y_expected):
        return np.linalg.norm(Y - Y_expected, ord=1)

def calculate_error(true_matrix, predicted_matrix):
    # Calculate the Mean Squared Error between the true matrix and the predicted matrix
    mse = np.mean((true_matrix - predicted_matrix)**2)
    rmse = np.sqrt(mse)
    return mse, rmse


def main():
    X = read_image('x1.bmp')
    Y = read_image('y4.bmp')

    print('X shape:', X.shape)
    print('Y shape:', Y.shape)

    # Append a row of ones to the input matrix
    m = X.shape[1]
    X = np.vstack((X, np.ones((1, m))))
    print('X:', X)

    # Moore-Penrose method
    A1 = CalculateOperator(X, Y, pimm.pseudoInverseMatrix_MoorePenrose, eps=1e-60, delta=1000)
    Y1 = ApplyOperator(X, A1)
    ShowImage(Y1)

    # Greville method
    A2 = CalculateOperator(X, Y, pimm.pseudoInverseMatrix_Greville, eps=1e-60, delta=None)
    Y2 = ApplyOperator(X, A2)
    ShowImage(Y2)

    # SVD method
    A3 = CalculateOperator(X, Y, pimm.pseudoInverseMatrix_SVD, eps=1e-60, delta=None)
    Y3 = ApplyOperator(X, A3)
    ShowImage(Y3)

    print('Error 1 Moore-Penrose:', CalculateError1(Y, Y1))
    print('Error 1 Greville:', CalculateError1(Y, Y2))

    print('MSE, RMSE Moore-Penrose:', calculate_error(Y, Y1))
    print('MSE, RMSE Greville:', calculate_error(Y, Y2))

if __name__ == "__main__":
    main()