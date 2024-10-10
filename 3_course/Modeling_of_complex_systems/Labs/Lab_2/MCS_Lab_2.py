import numpy as np
import imageio.v2 as imageio
import matplotlib.pyplot as plt
import PseudoInverseMatrixMethods as pimm

def readImage(filename):
    # Read the .bmp input and output files as a numpy 2D matrix
    X = imageio.imread(filename)
    # Convert the image to a NumPy array
    return np.array(X)

def calculateOperator(X, Y, inversion_function, V=None, eps=1e-2, delta=10):
    if V is None:
        V = np.zeros((Y.shape[0], X.shape[0]))

    X_pinv = inversion_function(X, eps=eps, delta=delta)
    # assert pimm.isPsevdoInversed(X, X_pinv)
        
    YX_pinv = Y @ X_pinv
    Z = np.eye(X.shape[0]) - X @ X_pinv
    
    VZ = V @ Z.T
    A = YX_pinv + VZ
    return A

def applyOperator(X_, A_):
    return A_ @ X_

def showImage(X_):
    plt.imshow(X_, cmap='gray')
    plt.show()


def calculateError1(Y, Y_expected):
        return np.linalg.norm(Y - Y_expected, ord=1)

def calculateError(true_matrix, predicted_matrix):
    # Calculate the Mean Squared Error between the true matrix and the predicted matrix
    mse = np.mean((true_matrix - predicted_matrix)**2)
    rmse = np.sqrt(mse)
    return mse, rmse


def main():
    X = readImage('x1.bmp')
    Y = readImage('y4.bmp')

    print('X shape:', X.shape)
    print('Y shape:', Y.shape)

    # Append a row of ones to the input matrix
    m = X.shape[1]
    X = np.vstack((X, np.ones((1, m))))
    print('X:', X)

    # Moore-Penrose method
    A1 = calculateOperator(X, Y, pimm.pseudoInverseMatrix_MoorePenrose, eps=1e-60, delta=1000)
    Y1 = applyOperator(X, A1)
    showImage(Y1)

    # ??? method
    A2 = calculateOperator(X, Y, pimm.pseudoInverseMatrix_MoorePenrose_GradientDescent, eps=1e-60, delta=1000)
    Y2 = applyOperator(X, A2)
    showImage(Y2)

    # Greville method
    A3 = calculateOperator(X, Y, pimm.pseudoInverseMatrix_Greville, eps=1e-60, delta=None)
    Y3 = applyOperator(X, A3)
    showImage(Y3)

    # SVD method
    A4 = calculateOperator(X, Y, pimm.pseudoInverseMatrix_SVD, eps=1e-60, delta=None)
    # Y4 = applyOperator(X, A4)
    # showImage(Y4)

    print('Error 1 Moore-Penrose:', calculateError1(Y, Y1))
    print('Error 1 Greville:', calculateError1(Y, Y2))

    print('MSE, RMSE Moore-Penrose:', calculateError(Y, Y1))
    print('MSE, RMSE Greville:', calculateError(Y, Y2))

if __name__ == "__main__":
    main()