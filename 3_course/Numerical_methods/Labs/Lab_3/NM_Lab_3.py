import numpy as np
import time
import os
import sys
import matplotlib.pyplot as plt

sys.path.append(os.path.dirname(os.path.dirname(__file__)))  # Add parent directory to path
from Lab_2.NM_Lab_2_v2 import gaussian_elimination

def f(x, y):
    return np.array([
        np.sin(x - y) - x * y + 1,
        x**2 - y**2 - 0.75
    ])

def plot_system():
    # Create grid of points
    x = np.linspace(-2, 2, 300)
    y = np.linspace(-2, 2, 300)
    X, Y = np.meshgrid(x, y)
    
    # Calculate functions for each point
    Z1 = np.sin(X - Y) - X * Y + 1
    Z2 = X**2 - Y**2 - 0.75
    
    # Create figure and axis
    plt.figure(figsize=(10, 8))
    
    # Plot contours for first equation
    plt.contour(X, Y, Z1, levels=[0], colors='blue', label='sin(x-y) - xy + 1 = 0')
    
    # Plot contours for second equation
    plt.contour(X, Y, Z2, levels=[0], colors='red', label='x² - y² = 0.75')
    
    plt.grid(True)
    plt.xlabel('x')
    plt.ylabel('y')
    plt.title('System of Equations')
    plt.legend()
    plt.axis('equal')
    
    # Add gridlines and zero lines
    plt.axhline(y=0, color='k', linestyle='--', alpha=0.3)
    plt.axvline(x=0, color='k', linestyle='--', alpha=0.3)
    
    plt.savefig('system_plot.png')
    plt.show()  # This will display the plot
    plt.close()

def jacobian(x, y):
    return np.array([
        [np.cos(x - y) - y, -np.cos(x - y) - x],
        [2*x, -2*y]
    ])

def newton_method(x0, y0, epsilon, log_file):
    x, y = x0, y0
    iteration = 0

    log_file.write(f"Initial approximation: x = {x}, y = {y}\n")
    log_file.write("Iteration,x,y,||F(x,y)||\n")

    while True:
        J = jacobian(x, y)
        F = f(x, y)
        
        norm_F = np.linalg.norm(F)
        log_file.write(f"{iteration},{x},{y},{norm_F}\n")

        z = gaussian_elimination(J, F)

        x -= z[0]
        y -= z[1]
        
        iteration += 1

        if np.linalg.norm(z) < epsilon:
            log_file.write(f"Solution was found on iteration {iteration}\n")
            return x, y

def main():
    print("Розв'язання системи рівнянь методом Ньютона:")
    print("sin(x - y) - x * y = -1")
    print("x^2 - y^2 = 0.75")
    
    # Plot the system before getting input
    print("\nСтворення графіку системи рівнянь...")
    plot_system()
    print("Графік збережено у файлі: system_plot.png")
    
    x0 = float(input("\nВведіть початкове наближення для x: "))
    y0 = float(input("Введіть початкове наближення для y: "))
    epsilon = float(input("Введіть точність (епсилон): "))
    
    log_filename = "newton_method_log.csv"
    with open(log_filename, "w") as log_file:
        start_time = time.time()
        x, y = newton_method(x0, y0, epsilon, log_file)
        end_time = time.time()
    
    print(f"\nРозв'язок:")
    print(f"x = {x}")
    print(f"y = {y}")
    print(f"\nЧас виконання: {end_time - start_time:.6f} секунд")
    
    # Перевірка розв'язку
    F = f(x, y)
    print(f"\nПеревірка (має бути близько до нуля):")
    print(f"f1(x,y) = {F[0]}")
    print(f"f2(x,y) = {F[1]}")
    
    print(f"\nЛог ітерацій збережено у файлі: {os.path.abspath(log_filename)}")

if __name__ == "__main__":
    main()