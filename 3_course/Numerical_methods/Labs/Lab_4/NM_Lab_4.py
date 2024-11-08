import numpy as np
import matplotlib.pyplot as plt
import time

# Визначаємо функцію f(x)
def f(x):
    return 5 / (1 + x**2)

# Функція для знаходження рівновіддалених вузлів
def equidistant_nodes(n):
    return np.linspace(-1, 1, n + 1)

# Функція для знаходження вузлів Чебишова
def chebyshev_nodes(n):
    return np.cos((2 * np.arange(1, n + 2) - 1) * np.pi / (2 * (n + 1)))

# Інтерполяція методом Лагранжа
def lagrange_interpolation(x, nodes, values):
    n = len(nodes)
    L = np.zeros_like(x)
    for i in range(n):
        li = np.ones_like(x)
        for j in range(n):
            if i != j:
                li *= (x - nodes[j]) / (nodes[i] - nodes[j])
        L += values[i] * li
    return L

# Інтерполяція методом Ньютона
def newton_interpolation(x, nodes, values):
    n = len(nodes)
    divided_diffs = np.copy(values)
    for i in range(1, n):
        for j in range(n - 1, i - 1, -1):
            divided_diffs[j] = (divided_diffs[j] - divided_diffs[j - 1]) / (nodes[j] - nodes[j - i])
    
    # Обчислення полінома Ньютона
    N = np.ones_like(x) * divided_diffs[0]
    for i in range(1, n):
        term = divided_diffs[i]
        for j in range(i):
            term *= (x - nodes[j])
        N += term
    return N

# Основний блок програми
def main():
    # Введення степеня полінома
    degree = int(input("Введіть степінь інтерполяційного полінома: "))
    
    # Точки для побудови графіка
    x = np.linspace(-1, 1, 500)
    y_true = f(x)

    # Інтерполяція з рівновіддаленими вузлами
    eq_nodes = equidistant_nodes(degree)
    eq_values = f(eq_nodes)

    start_time = time.time()
    y_lagrange_eq = lagrange_interpolation(x, eq_nodes, eq_values)
    lagrange_eq_time = time.time() - start_time

    start_time = time.time()
    y_newton_eq = newton_interpolation(x, eq_nodes, eq_values)
    newton_eq_time = time.time() - start_time

    # Інтерполяція з вузлами Чебишова
    ch_nodes = chebyshev_nodes(degree)
    ch_values = f(ch_nodes)

    start_time = time.time()
    y_lagrange_ch = lagrange_interpolation(x, ch_nodes, ch_values)
    lagrange_ch_time = time.time() - start_time

    start_time = time.time()
    y_newton_ch = newton_interpolation(x, ch_nodes, ch_values)
    newton_ch_time = time.time() - start_time

    # Обчислення похибок
    lagrange_eq_error = np.abs(y_true - y_lagrange_eq)
    newton_eq_error = np.abs(y_true - y_newton_eq)
    lagrange_ch_error = np.abs(y_true - y_lagrange_ch)
    newton_ch_error = np.abs(y_true - y_newton_ch)

    # Виведення часу виконання
    print(f"Час виконання Лагранжа з рівновіддаленими вузлами: {lagrange_eq_time:.6f} секунд")
    print(f"Час виконання Ньютона з рівновіддаленими вузлами: {newton_eq_time:.6f} секунд")
    print(f"Час виконання Лагранжа з вузлами Чебишова: {lagrange_ch_time:.6f} секунд")
    print(f"Час виконання Ньютона з вузлами Чебишова: {newton_ch_time:.6f} секунд")

    # Побудова графіків
    plt.figure(figsize=(12, 10))

    # Графік функції та інтерполяційних поліномів з рівновіддаленими вузлами
    plt.subplot(2, 2, 1)
    plt.plot(x, y_true, label="f(x)", color="black")
    plt.plot(x, y_lagrange_eq, label="Лагранж (рівновіддалені вузли)", linestyle="--")
    plt.plot(x, y_newton_eq, label="Ньютон (рівновіддалені вузли)", linestyle=":")
    plt.scatter(eq_nodes, eq_values, color="red", zorder=5, label="Рівновіддалені вузли")
    plt.legend()
    plt.title("Інтерполяція з рівновіддаленими вузлами")
    plt.xlabel("x")
    plt.ylabel("y")

    # Графік функції та інтерполяційних поліномів з вузлами Чебишова
    plt.subplot(2, 2, 2)
    plt.plot(x, y_true, label="f(x)", color="black")
    plt.plot(x, y_lagrange_ch, label="Лагранж (вузли Чебишова)", linestyle="--")
    plt.plot(x, y_newton_ch, label="Ньютон (вузли Чебишова)", linestyle=":")
    plt.scatter(ch_nodes, ch_values, color="blue", zorder=5, label="Вузли Чебишова")
    plt.legend()
    plt.title("Інтерполяція з вузлами Чебишова")
    plt.xlabel("x")
    plt.ylabel("y")

    # Графік похибок для рівновіддалених вузлів
    plt.subplot(2, 2, 3)
    plt.plot(x, lagrange_eq_error, label="Похибка Лагранжа (рівновіддалені вузли)", linestyle="--")
    plt.plot(x, newton_eq_error, label="Похибка Ньютона (рівновіддалені вузли)", linestyle=":")
    plt.legend()
    plt.title("Похибки для рівновіддалених вузлів")
    plt.xlabel("x")
    plt.ylabel("Похибка")

    # Графік похибок для вузлів Чебишова
    plt.subplot(2, 2, 4)
    plt.plot(x, lagrange_ch_error, label="Похибка Лагранжа (вузли Чебишова)", linestyle="--")
    plt.plot(x, newton_ch_error, label="Похибка Ньютона (вузли Чебишова)", linestyle=":")
    plt.legend()
    plt.title("Похибки для вузлів Чебишова")
    plt.xlabel("x")
    plt.ylabel("Похибка")

    plt.tight_layout()
    plt.show()

if __name__ == "__main__":
    main()
