import numpy as np
import csv
from datetime import datetime
import time

def read_file(file_name):
    with open(file_name, 'r') as file:
        lines = file.readlines()
        input_data = []
        for line in lines:
            values = line.strip().split()
            row = [float(value) for value in values]
            input_data.append(row)
    return np.array(input_data).T

def finite_diff(y_vec_func, b_vec, b_values, delta=1e-5):
    n = len(y_vec_func(b_values))
    m = len(b_vec)
    deriv_matrix = np.zeros((n, m))
    
    for j in range(m):
        original_value = b_values[b_vec[j]]
        
        # Calculate y with a positive perturbation
        b_values[b_vec[j]] = original_value + delta
        y_plus = y_vec_func(b_values)
        
        # Calculate y with a negative perturbation
        b_values[b_vec[j]] = original_value - delta
        y_minus = y_vec_func(b_values)
        
        # Restore original value
        b_values[b_vec[j]] = original_value
        
        # Calculate the finite difference derivative
        deriv_matrix[:, j] = (y_plus - y_minus) / (2 * delta)
    
    return deriv_matrix

def get_u_matr(a_matr, b_matr, u_matr, h):
    b_arrayed = np.array(b_matr)
    k1 = h * (np.dot(a_matr, u_matr) + b_arrayed)
    k2 = h * (np.dot(a_matr, u_matr + k1 / 2) + b_arrayed)
    k3 = h * (np.dot(a_matr, u_matr + k2 / 2) + b_arrayed)
    k4 = h * (np.dot(a_matr, u_matr + k3) + b_arrayed)
    return u_matr + (k1 + 2 * k2 + 2 * k3 + k4) / 6

def get_y(a_matr, y_cur, h):
    k1 = h * np.dot(a_matr, y_cur)
    k2 = h * np.dot(a_matr, y_cur + k1 / 2)
    k3 = h * np.dot(a_matr, y_cur + k2 / 2)
    k4 = h * np.dot(a_matr, y_cur + k3)
    return y_cur + (k1 + 2 * k2 + 2 * k3 + k4) / 6

def init_matr(params):
    c1, c2, c3, c4, m1, m2, m3 = params['c1'], params['c2'], params['c3'], params['c4'], params['m1'], params['m2'], params['m3']
    matr = [
        [0, 1, 0, 0, 0, 0],
        [-(c2 + c1) / m1, 0, c2 / m1, 0, 0, 0],
        [0, 0, 0, 1, 0, 0],
        [c2 / m2, 0, -(c2 + c3) / m2, 0, c3 / m2, 0],
        [0, 0, 0, 0, 0, 1],
        [0, 0, c3 / m3, 0, -(c4 + c3) / m3, 0]
    ]
    return np.array(matr)

def approximate(y_matr, params, beta_symbols, beta_values, eps, h=0.2):
    start_time = time.time()
    
    # Create a log file with timestamp
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    log_filename = f'approximation_log_{timestamp}.csv'
    
    # Initialize the CSV file with headers
    with open(log_filename, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        headers = ['iteration'] + beta_symbols + ['quality_degree']
        writer.writerow(headers)
    
    all_params = {**params, **beta_values}
    a_matrix = init_matr(all_params)
    
    beta_vector = np.array([beta_values[beta_symbols[0]], beta_values[beta_symbols[1]], beta_values[beta_symbols[2]]])
    iteration = 0
    
    def y_vec_func(b_values):
        all_params.update(b_values)
        a_matrix = init_matr(all_params)
        return a_matrix @ y_approximation
    
    while True:
        all_params.update(beta_values)
        a_complete = init_matr(all_params)
        
        u_matr = np.zeros((6, 3))
        quality_degree = 0
        integral_part_inverse = np.zeros((3, 3))
        integral_part_mult = np.zeros((1, 3))
        y_approximation = y_matr[0]
        
        for i in range(len(y_matr)):
            b_derivative_matr = finite_diff(y_vec_func, beta_symbols, beta_values)
            integral_part_inverse += u_matr.T @ u_matr
            integral_part_mult += u_matr.T @ (y_matr[i] - y_approximation)
            quality_degree += (y_matr[i] - y_approximation).T @ (y_matr[i] - y_approximation)
            
            u_matr = get_u_matr(a_complete, b_derivative_matr, u_matr, h)
            y_approximation = get_y(a_complete, y_approximation, h)
        
        integral_part_inverse *= h
        integral_part_mult *= h
        quality_degree *= h
        
        # Log the current iteration data
        with open(log_filename, 'a', newline='') as csvfile:
            writer = csv.writer(csvfile)
            writer.writerow([iteration] + list(beta_vector) + [quality_degree])
        
        delta_beta = np.linalg.inv(integral_part_inverse) @ integral_part_mult.flatten()
        beta_vector += delta_beta
        
        beta_values = {beta_symbols[i]: beta_vector[i] for i in range(3)}
        
        if quality_degree < eps:
            end_time = time.time()
            execution_time = end_time - start_time
            return beta_values, iteration + 1, execution_time
            
        iteration += 1

def main():
    input_data = read_file('./y4.txt')
    params = {'c1': 0.14, 'c2': 0.3, 'c4': 0.12, 'm1': 12}
    to_approx = {'m2': 21, 'c3': 0.15, 'm3': 11}
    
    result, iterations, execution_time = approximate(input_data, params, ['m2', 'c3', 'm3'], to_approx, 1e-6)
    
    print("\nExecution Summary:")
    print("-----------------")
    print(f"Total iterations: {iterations}")
    print(f"Execution time: {execution_time:.2f} seconds")
    print("\nFinal approximation:")
    for param, value in result.items():
        print(f"{param}: {value:.6f}")

if __name__ == "__main__":
    main()