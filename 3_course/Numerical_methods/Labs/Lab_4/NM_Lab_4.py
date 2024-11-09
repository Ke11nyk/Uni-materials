import numpy as np
import matplotlib.pyplot as plt
from time import time
import math
from scipy.optimize import fsolve

def f(x):
    """Задана функція f(x) = 5 / (1 + x^2)"""
    return 5 / (1 + x**2)

def df(x):
    """Похідна функції f(x)"""
    return -10*x / (1 + x**2)**2

def find_monotonicity(a, b, steps=1000):
    """Перевірка монотонності функції на відрізку [a, b] та повернення інтервалів монотонності."""
    x = np.linspace(a, b, steps)
    derivative = df(x)
    
    increasing = all(d >= 0 for d in derivative)
    decreasing = all(d <= 0 for d in derivative)
    
    # Якщо функція є монотонною на всьому відрізку, повертаємо це
    if increasing:
        return "monotonic increasing", [(a, b)]
    elif decreasing:
        return "monotonic decreasing", [(a, b)]
    
    # Якщо функція не монотонна, визначаємо інтервали між точками зміни знаку похідної
    intervals = []
    start = a
    
    for i in range(1, len(x)):
        # Якщо похідна змінює знак
        if derivative[i-1] * derivative[i] <= 0:
            # Додаємо інтервал між попередньою точкою та поточною
            end = (x[i-1] + x[i]) / 2
            intervals.append((start, end))
            start = end  # Оновлюємо початок наступного інтервалу
    
    # Додаємо останній інтервал до кінця
    intervals.append((start, b))
    
    return "non-monotonic", intervals

def chebyshev_nodes(a, b, n):
    """Обчислення вузлів Чебишова"""
    i = np.arange(n)
    return ((a + b) + (b - a) * np.cos((2*i + 1) * np.pi / (2*n))) / 2

def uniform_nodes(a, b, n):
    """Обчислення рівномірних вузлів"""
    return np.linspace(a, b, n)

def lagrange_interpolation(x, nodes, values):
    """Інтерполяція методом Лагранжа"""
    n = len(nodes)
    result = 0
    
    for i in range(n):
        term = values[i]
        for j in range(n):
            if i != j:
                denominator = nodes[i] - nodes[j]
                if abs(denominator) < 1e-10:
                    continue
                term *= (x - nodes[j])/denominator
        result += term
    
    return result

def print_divided_differences_table(x, y):
    """Виведення таблиці розділених різниць"""
    n = len(x)
    # Створюємо матрицю для зберігання різниць
    _ , f = divided_differences(x, y)
    
    # Виведення таблиці
    print("\nТаблиця розділених різниць:")
    print("-" * 100)
    header = ["x", "f(x)"]
    for i in range(1, n):
        header.append(f"f[x0...x{i}]")
    print(f"{' '.join(f'{h:>15}' for h in header)}")
    print("-" * 100)
    
    for i in range(n):
        row = [f"{x[i]:15.6f}", f"{f[i, 0]:15.6f}"]
        for j in range(1, n-i):
            row.append(f"{f[i, j]:15.6f}")
        print("".join(row))
    print("-" * 100)

def divided_differences(x, y):
    """Обчислення розділених різниць для методу Ньютона"""
    n = len(x)
    f = np.zeros((n, n))
    f[:, 0] = y
    
    for j in range(1, n):
        for i in range(n - j):
            f[i, j] = (f[i + 1, j - 1] - f[i, j - 1]) / (x[i + j] - x[i])
    
    return f[0], f

def newton_interpolation(x, nodes, values):
    """Інтерполяція методом Ньютона"""
    coef, _ = divided_differences(nodes, values)
    n = len(nodes)
    result = coef[0]
    
    for i in range(1, n):
        term = coef[i]
        for j in range(i):
            term *= (x - nodes[j])
        result += term
    
    return result

def calculate_errors(x_plot, y_exact, y_interpolated):
    """Обчислення абсолютної та відносної похибок"""
    absolute_error = np.abs(np.array(y_exact) - np.array(y_interpolated))
    relative_error = absolute_error / (np.array(y_exact) + 1e-10) * 100
    return absolute_error, relative_error

def inverse_interpolation(y_value, x_nodes, y_nodes, monotonicity):
    """
    Обернена інтерполяція для знаходження x такого, що f(x) = y_value
    використовуючи метод Лагранжа на кожному інтервалі монотонності.
    
    Args:
        y_value (float): значення y, для якого шукаємо x
        x_nodes (array): масив значень x у вузлах
        y_nodes (array): масив значень y у вузлах
        monotonicity (tuple): інформація про монотонність та інтервали локалізації
        
    Returns:
        list: знайдені значення x для кожного інтервалу
    """
    
    def target_function(x, nodes, values):
        return lagrange_interpolation(x, nodes, values) - y_value
    
    if not isinstance(monotonicity, tuple):
        x_initial_guess = (x_nodes[0] + x_nodes[-1]) / 2
        x_solution = fsolve(lambda x: target_function(x, x_nodes, y_nodes), x_initial_guess)
        return [x_solution[0]]
    
    intervals = monotonicity[1]
    solutions = []
    
    for interval in intervals:
        # Вибираємо точки та значення для поточного інтервалу
        x_sub_nodes = [x for x in x_nodes if interval[0] <= x <= interval[1]]
        y_sub_nodes = [y for x, y in zip(x_nodes, y_nodes) if interval[0] <= x <= interval[1]]
        
        if len(x_sub_nodes) < 2:
            continue  # Пропускаємо інтервал, якщо недостатньо точок для інтерполяції
        
        # Початкове значення для fsolve на основі середини інтервалу
        x_initial_guess = (interval[0] + interval[1]) / 2
        try:
            x_solution = fsolve(lambda x: target_function(x, x_sub_nodes, y_sub_nodes), x_initial_guess)
            if interval[0] <= x_solution[0] <= interval[1]:
                solutions.append(x_solution[0])
        except ValueError:
            pass  # Пропускаємо інтервали, де корінь не знайдено
    
    return solutions

def main():
    # Введення степеня інтерполяційного полінома
    n = int(input("Введіть степінь інтерполяційного полінома: "))
    
    # Визначення відрізка
    a, b = -1, 1
    
    # Створення вузлів
    uniform = uniform_nodes(a, b, n+1)
    chebyshev = chebyshev_nodes(a, b, n+1)
    
    # Обчислення значень функції у вузлах
    uniform_values = [f(x) for x in uniform]
    chebyshev_values = [f(x) for x in chebyshev]

    # Виведення таблиць розділених різниць
    print("\nДля рівномірних вузлів:")
    print_divided_differences_table(uniform, uniform_values)
    
    print("\nДля вузлів Чебишова:")
    print_divided_differences_table(chebyshev, chebyshev_values)
    
    # Створення точок для побудови графіків
    x_plot = np.linspace(a, b, 1000)
    y_plot = [f(x) for x in x_plot]
    
    # Час роботи та обчислення інтерполяцій
    print("\nЧас роботи методів інтерполяції:")
    
    # Рівномірні вузли
    start_time = time()
    y_lagrange_uniform = [lagrange_interpolation(x, uniform, uniform_values) for x in x_plot]
    print(f"Метод Лагранжа (рівномірні вузли): {time() - start_time:.6f} сек")
    
    start_time = time()
    y_newton_uniform = [newton_interpolation(x, uniform, uniform_values) for x in x_plot]
    print(f"Метод Ньютона (рівномірні вузли): {time() - start_time:.6f} сек")
    
    # Вузли Чебишова
    start_time = time()
    y_lagrange_chebyshev = [lagrange_interpolation(x, chebyshev, chebyshev_values) for x in x_plot]
    print(f"Метод Лагранжа (вузли Чебишова): {time() - start_time:.6f} сек")
    
    start_time = time()
    y_newton_chebyshev = [newton_interpolation(x, chebyshev, chebyshev_values) for x in x_plot]
    print(f"Метод Ньютона (вузли Чебишова): {time() - start_time:.6f} сек")
    
    # Обчислення похибок
    abs_err_lagrange_uniform, rel_err_lagrange_uniform = calculate_errors(x_plot, y_plot, y_lagrange_uniform)
    abs_err_newton_uniform, rel_err_newton_uniform = calculate_errors(x_plot, y_plot, y_newton_uniform)
    abs_err_lagrange_chebyshev, rel_err_lagrange_chebyshev = calculate_errors(x_plot, y_plot, y_lagrange_chebyshev)
    abs_err_newton_chebyshev, rel_err_newton_chebyshev = calculate_errors(x_plot, y_plot, y_newton_chebyshev)
    
    # Створення графіків (2 фігури: для інтерполяції та для похибок)
    # Перша фігура - інтерполяція
    plt.figure(figsize=(15, 10))
    
    plt.subplot(2, 2, 1)
    plt.plot(x_plot, y_plot, 'k-', label='Точна функція')
    plt.plot(x_plot, y_lagrange_uniform, 'r--', label='Лагранж')
    plt.plot(x_plot, y_newton_uniform, 'b:', label='Ньютон')
    plt.plot(uniform, uniform_values, 'go', label='Вузли')
    plt.title('Інтерполяція на рівномірних вузлах')
    plt.legend()
    plt.grid(True)
    
    plt.subplot(2, 2, 2)
    plt.plot(x_plot, y_plot, 'k-', label='Точна функція')
    plt.plot(x_plot, y_lagrange_chebyshev, 'r--', label='Лагранж')
    plt.plot(x_plot, y_newton_chebyshev, 'b:', label='Ньютон')
    plt.plot(chebyshev, chebyshev_values, 'go', label='Вузли')
    plt.title('Інтерполяція на вузлах Чебишова')
    plt.legend()
    plt.grid(True)
    
    # Значення y для оберненої інтерполяції
    y_values = np.linspace(1.0, 5.0, 5)  # 5 точок від 1 до 5

    # Перевірка монотонності
    monotonicity = find_monotonicity(a, b)
    print("\nАналіз монотонності:")
    if isinstance(find_monotonicity(a, b), tuple):
        print("Функція не монотонна")
        print("Точки локалізації:", monotonicity[1])
    else:
        print(f"Функція {monotonicity}")

    # Виклик функції inverse_interpolation для кожного значення y в y_values
    x_inverse_uniform = [inverse_interpolation(y, uniform, uniform_values, monotonicity) for y in y_values]
    x_inverse_chebyshev = [inverse_interpolation(y, chebyshev, chebyshev_values, monotonicity) for y in y_values]

    # Графік точної оберненої функції
    y_plot_inverse = np.linspace(1, 5, 1000)
    x_plot_inverse = [-np.sqrt(5/y - 1) for y in y_plot_inverse]

    # Розгортання списку x_inverse_uniform для побудови графіка
    y_plot_inverse_uniform = []
    x_plot_inverse_uniform = []

    for y_val, x_vals in zip(y_values, x_inverse_uniform):
        if isinstance(x_vals, list):  # Якщо отримали кілька значень для одного y
            for x_val in x_vals:
                y_plot_inverse_uniform.append(y_val)
                x_plot_inverse_uniform.append(x_val)
        else:  # Якщо отримали одне значення для y
            y_plot_inverse_uniform.append(y_val)
            x_plot_inverse_uniform.append(x_vals)

    # Розгортання списку x_inverse_chebyshev для побудови графіка
    y_plot_inverse_chebyshev = []
    x_plot_inverse_chebyshev = []

    for y_val, x_vals in zip(y_values, x_inverse_chebyshev):
        if isinstance(x_vals, list):  # Якщо отримали кілька значень для одного y
            for x_val in x_vals:
                y_plot_inverse_chebyshev.append(y_val)
                x_plot_inverse_chebyshev.append(x_val)
        else:  # Якщо отримали одне значення для y
            y_plot_inverse_chebyshev.append(y_val)
            x_plot_inverse_chebyshev.append(x_vals)

    plt.subplot(2, 2, 3)
    plt.plot(y_plot_inverse, x_plot_inverse, 'k-', label='Точна функція')
    plt.plot(y_plot_inverse_uniform, x_plot_inverse_uniform, 'ro', label='Обернена (рівн.)')
    plt.title('Обернена інтерполяція (рівномірні вузли)')
    plt.legend()
    plt.grid(True)

    plt.subplot(2, 2, 4)
    plt.plot(y_plot_inverse, x_plot_inverse, 'k-', label='Точна функція')
    plt.plot(y_plot_inverse_chebyshev, x_plot_inverse_chebyshev, 'ro', label='Обернена (рівн.)')
    plt.title('Обернена інтерполяція (вузли Чебишова)')
    plt.legend()
    plt.grid(True)
    
    plt.tight_layout()
    plt.savefig('interpolation_report.png')
    
    # Друга фігура - похибки
    plt.figure(figsize=(15, 10))
    
    # Абсолютні похибки для рівномірних вузлів
    plt.subplot(2, 2, 1)
    plt.plot(x_plot, abs_err_lagrange_uniform, 'r-', label='Лагранж')
    plt.plot(x_plot, abs_err_newton_uniform, 'b--', label='Ньютон')
    plt.title('Абсолютна похибка (рівномірні вузли)')
    plt.legend()
    plt.grid(True)
    plt.ylabel('Похибка')
    
    # Абсолютні похибки для вузлів Чебишова
    plt.subplot(2, 2, 2)
    plt.plot(x_plot, abs_err_lagrange_chebyshev, 'r-', label='Лагранж')
    plt.plot(x_plot, abs_err_newton_chebyshev, 'b--', label='Ньютон')
    plt.title('Абсолютна похибка (вузли Чебишова)')
    plt.legend()
    plt.grid(True)
    
    # Відносні похибки для рівномірних вузлів
    plt.subplot(2, 2, 3)
    plt.plot(x_plot, rel_err_lagrange_uniform, 'r-', label='Лагранж')
    plt.plot(x_plot, rel_err_newton_uniform, 'b--', label='Ньютон')
    plt.title('Відносна похибка (%) (рівномірні вузли)')
    plt.legend()
    plt.grid(True)
    plt.ylabel('Похибка (%)')
    
    # Відносні похибки для вузлів Чебишова
    plt.subplot(2, 2, 4)
    plt.plot(x_plot, rel_err_lagrange_chebyshev, 'r-', label='Лагранж')
    plt.plot(x_plot, rel_err_newton_chebyshev, 'b--', label='Ньютон')
    plt.title('Відносна похибка (%) (вузли Чебишова)')
    plt.legend()
    plt.grid(True)
    
    plt.tight_layout()
    plt.savefig('errors_report.png')
    
    # Виведення максимальних похибок
    print("\nМаксимальні абсолютні похибки:")
    print(f"Лагранж (рівномірні вузли): {np.max(abs_err_lagrange_uniform):.6f}")
    print(f"Ньютон (рівномірні вузли): {np.max(abs_err_newton_uniform):.6f}")
    print(f"Лагранж (вузли Чебишова): {np.max(abs_err_lagrange_chebyshev):.6f}")
    print(f"Ньютон (вузли Чебишова): {np.max(abs_err_newton_chebyshev):.6f}")
    
    print("\nМаксимальні відносні похибки (%):")
    print(f"Лагранж (рівномірні вузли): {np.max(rel_err_lagrange_uniform):.6f}")
    print(f"Ньютон (рівномірні вузли): {np.max(rel_err_newton_uniform):.6f}")
    print(f"Лагранж (вузли Чебишова): {np.max(rel_err_lagrange_chebyshev):.6f}")
    print(f"Ньютон (вузли Чебишова): {np.max(rel_err_newton_chebyshev):.6f}")

if __name__ == "__main__":
    main()