import numpy as np
import matplotlib.pyplot as plt

# Точна функція та її похідні
def f(x):
    return 5 / (1 + x**2)

def df(x):
    return -10 * x / (1 + x**2)**2

def df2(x):
    return 10 * (3 * x**2 - 1) / (1 + x**2)**3

# Розв’язання системи
def solve_tridiagonal(A, b):
    n = len(b)
    alpha = np.zeros(n)
    beta = np.zeros(n)

    alpha[0] = -A[0, 1] / A[0, 0]
    beta[0] = b[0] / A[0, 0]
    for i in range(1, n):
        denom = A[i, i] + (A[i, i-1] * alpha[i-1])
        if i < n-1:
            alpha[i] = -A[i, i+1] / denom
        beta[i] = (b[i] - A[i, i-1] * beta[i-1]) / denom

    m = np.zeros(n)
    m[-1] = beta[-1]
    for i in range(n-2, -1, -1):
        m[i] = alpha[i] * m[i+1] + beta[i]
    return m

# Обчислення кубічного сплайна
def spline_interpolation(x_nodes, f_nodes, x_dense):
    n = len(x_nodes)
    h = [x_nodes[i] - x_nodes[i-1] for i in range(1, n)]
    
    # Формування матриці A та вектора b = Hf
    A = np.zeros((n-2, n-2))
    b = np.zeros(n-2)

    for i in range(n-2):
        if i > 0:
            A[i, i-1] = h[i] / 6
        A[i, i] = (h[i] + h[i+1]) / 3
        if i < n-3:
            A[i, i+1] = h[i+1] / 6

        b[i] = (f_nodes[i+2] - f_nodes[i+1]) / h[i+1] - (f_nodes[i+1] - f_nodes[i]) / h[i]

    m_internal = solve_tridiagonal(A, b)
    m = np.zeros(n)
    m[1:-1] = m_internal

    # Функції сплайна та його похідних
    def spline(x_val):
        for i in range(1, n):
            if x_nodes[i-1] <= x_val <= x_nodes[i]:
                hi = x_nodes[i] - x_nodes[i-1]
                a = (x_nodes[i] - x_val) / hi
                b = (x_val - x_nodes[i-1]) / hi
                return (
                    a * f_nodes[i-1]
                    + b * f_nodes[i]
                    + (a**3 - a) * m[i-1] * hi**2 / 6
                    + (b**3 - b) * m[i] * hi**2 / 6
                )
        return None

    def spline_df(x_val):
        for i in range(1, n):
            if x_nodes[i-1] <= x_val <= x_nodes[i]:
                hi = x_nodes[i] - x_nodes[i-1]
                a = (x_nodes[i] - x_val) / hi
                b = (x_val - x_nodes[i-1]) / hi
                return (
                    -f_nodes[i-1] / hi
                    + f_nodes[i] / hi
                    + (3 * a**2 - 1) * m[i-1] * hi / 6
                    + (3 * b**2 - 1) * m[i] * hi / 6
                )
        return None

    def spline_df2(x_val):
        for i in range(1, n):
            if x_nodes[i-1] <= x_val <= x_nodes[i]:
                hi = x_nodes[i] - x_nodes[i-1]
                a = (x_nodes[i] - x_val) / hi
                b = (x_val - x_nodes[i-1]) / hi
                return m[i-1] * a + m[i] * b
        return None

    f_spline = np.array([spline(x) for x in x_dense])
    f_spline_first = np.array([spline_df(x) for x in x_dense])
    f_spline_second = np.array([spline_df2(x) for x in x_dense])

    return f_spline, f_spline_first, f_spline_second

# Основна функція
def main():
    n = int(input("Введіть кількість вузлів (n >= 4): "))
    if n < 4:
        print("Кількість вузлів має бути не меншою за 4.")
        return

    x_nodes = np.linspace(-1, 1, n)
    f_nodes = f(x_nodes)

    x_dense = np.linspace(-1, 1, 500)
    f_exact = f(x_dense)
    f_exact_first = df(x_dense)
    f_exact_second = df2(x_dense)

    f_spline, f_spline_first, f_spline_second = spline_interpolation(x_nodes, f_nodes, x_dense)

    # Похибки
    error_function = np.abs(f_exact - f_spline)
    error_first_derivative = np.abs(f_exact_first - f_spline_first)
    error_second_derivative = np.abs(f_exact_second - f_spline_second)

    # Побудова графіків
    plt.figure(figsize=(12, 12))

    # Точна функція і сплайн
    plt.subplot(2, 2, 1)
    plt.plot(x_dense, f_exact, label="Точна функція", color="blue")
    plt.plot(x_dense, f_spline, label="Сплайн", color="orange", linestyle="--")
    plt.scatter(x_nodes, f_nodes, color="red", label="Вузли")
    plt.title("Функція та кубічний сплайн")
    plt.legend()
    plt.grid()

    # Перша похідна
    plt.subplot(2, 2, 2)
    plt.plot(x_dense, f_exact_first, label="Точна перша похідна", color="blue")
    plt.plot(x_dense, f_spline_first, label="Перша похідна сплайна", color="orange", linestyle="--")
    plt.title("Перша похідна")
    plt.legend()
    plt.grid()

    # Друга похідна
    plt.subplot(2, 2, 3)
    plt.plot(x_dense, f_exact_second, label="Точна друга похідна", color="blue")
    plt.plot(x_dense, f_spline_second, label="Друга похідна сплайна", color="orange", linestyle="--")
    plt.title("Друга похідна")
    plt.legend()
    plt.grid()

    # Похибки
    plt.subplot(2, 2, 4)
    plt.plot(x_dense, error_function, label="Похибка функції", color="blue")
    plt.plot(x_dense, error_first_derivative, label="Похибка першої похідної", color="green")
    plt.plot(x_dense, error_second_derivative, label="Похибка другої похідної", color="purple")
    plt.title("Похибки")
    plt.legend()
    plt.grid()

    plt.tight_layout()
    plt.savefig('spline_analysis.png')
    plt.show()

# Виклик функції
if __name__ == "__main__":
    main()