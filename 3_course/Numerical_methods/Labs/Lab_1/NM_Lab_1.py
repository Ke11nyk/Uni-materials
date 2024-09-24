import numpy as np
import matplotlib.pyplot as plt

# Функція для визначення непереревності функції
def continuity(f, a, b, epsilon=1e-4):
    if not callable(f):
        raise ValueError("The provided 'f' is not a callable function.")
    
    def limit(f, c, delta_x=1e-6):
        left_limit = f(c - delta_x)
        right_limit = f(c + delta_x)
        return left_limit, right_limit

    # Check if 'f' is defined at both 'a' and 'b'.
    if a <= b and (a is None or b is None):
        return False

    # Check the limits at 'a' and 'b'.
    left_limit_a, right_limit_a = limit(f, a)
    left_limit_b, right_limit_b = limit(f, b)

    # Check if the limits match the function value at 'a' and 'b'.
    if abs(left_limit_a - f(a)) > epsilon or abs(right_limit_a - f(a)) > epsilon or abs(left_limit_b - f(b)) > epsilon or abs(right_limit_b - f(b)) > epsilon:
        return False

    return True

# Функція для визначення знакосталості функції
def check_sign_changes(f, a, b, num_points=10000) -> bool:
    x_values = np.linspace(a, b, num_points)
    signs = [np.sign(f(x)) for x in x_values]

    sign_changes = False
    for i in range(1, len(signs)):
        if signs[i] != signs[i - 1]:
            sign_changes = True
            break
    
    return sign_changes


# Функція для визначення мінімуму та максимуму функції на інтервалі
def find_absolute_extrema(f, a, b, num_points=10000):
    x_values = np.linspace(a, b, num_points)
    y_values = [abs(f(x)) for x in x_values]

    absolute_max = max(y_values)
    absolute_min = min(y_values)

    return absolute_max, absolute_min

# Визначимо нашу функцію
def f(x):
    return x**2 * np.cos(2*x) - 1

# Визначимо похідну функції
def df(x):
    return 2*x * np.cos(2*x) - 2*x**2 * np.sin(2*x)

# Визначимо похідну другого порядку функції
def d2f(x):
    return 2 * np.cos(2*x) - 8 * x * np.sin(2*x) - 4 * x**2 * np.cos(2*x)

# Обчислення значень функції на проміжку [-9, 9]
x = np.linspace(-9, 9, 1000)
y = f(x)

# Побудова графіку функції
plt.plot(x, y)
plt.axhline(0, color='red', linestyle='--')
plt.xlabel('x')
plt.ylabel('f(x)')
plt.title('Графік функції f(x) = x^2cos(2x) - 1')
plt.grid(True)
plt.show()


def has_root(f, a, b) -> bool:
    if not callable(f):
        raise ValueError("f must be a callable function.")
    
    if a >= b:
        raise ValueError("The interval must satisfy 'a < b'.")
    
    fa = f(a)
    fb = f(b)
    
    if fa * fb < 0:
        return True
    else:
        return False


def g(x):
    return -1 / (4*x)

# Виразимо х з функції x + g(x)f(x)
def phi(x):
    return x + g(x) * f(x)

# Визначимо похідну функції
def dphi(x):
    return 1 + g(x) * (2*x*np.cos(2*x) - 2*x**2*np.sin(2*x)) + f(x) * (-1 / (4*x**2))


# Метод простої ітерації
def simple_iteration_method(phi, dphi, a, b, epsilon):
    print("Метод простої ітерації\n")

    x0 = (a + b) / 2  # Початкове наближення
    print("x0 =", x0)
    x = x0

    # Перевiримо достатнi умови збiжностi
    q = abs(dphi(a)) if abs(dphi(a)) > abs(dphi(b)) else abs(dphi(b))
    print("q =", q)
    delta = max(abs(a - x0), abs(b - x0))
    print("delta =", delta)
    condition_2_left = abs(phi(x0) - x0)
    print("|phi(x0) - x0| =", condition_2_left)
    condition_2_right = (1-q)*delta
    print("(1 - q) * delta =", condition_2_right)

    # Перевірка достатньої умови збіжності
    if q >= 1 or condition_2_left > condition_2_right:
        print("\nДостатня умова збіжності не виконується.")
        return None, 0
    else:
        print("\nДостатня умова збіжності виконується.")

    iterations = 0  # Лічильник ітерацій

    n = np.floor(np.log((b - a)/((1 - q) * epsilon)) / np.log(1 / q)) + 1
    n1 = np.floor(np.log(abs(phi(x) - x) / ((1 - q) * epsilon)) / np.log(1 / q)) + 1
    
    print(f"\nn >= {n} >= {n1}")
    
    print(f"\n{'Iteration':^10}{'x':^15}{'|x - phi(x)|':^15}")
    print("-" * 42)

    while True:
        if iterations > 100:
            print("Перевищено максимальну кількість ітерацій.")
            return x, iterations

        x_next = phi(x)
        diff = abs(x_next - x)
        iterations += 1

        if (iterations < 20):
            print(f"{iterations:^10}{x:^15.6f}{diff:^15.6e}")
        
        if abs(x_next - x) < epsilon:
            return x_next, iterations
        
        x = x_next


# Метод релаксації
def relaxation_method(f, df, a, b, epsilon):
    print("Метод релаксації\n")

    x0 = (a + b) / 2  # Початкове наближення
    print("x0 =", x0)
    print("df(x0) =", df(x0))
    x = x0
    
    # Обчислення m1, M1, q, tao
    M1, m1 = find_absolute_extrema(df, a, b)
    print("m1 =", m1)
    print("M1 =", M1)
    q = (M1 - m1) / (M1 + m1)
    print("q =", q)
    tao = 2 / (M1 + m1)
    print("tao =", tao)

    # Перевірка достатньої умови збіжності
    if m1 <= 0 or m1 >= M1 or m1 >= np.abs(df(x)) or M1 <= np.abs(df(x)):
        print("\nДостатня умова збіжності не виконується.")
        return None, 0
    else:
        print("\nДостатня умова збіжності виконується.")

    sign = 1 if df(x) < 0 else -1
    
    iterations = 0  # Лічильник ітерацій
    
    # Create 50 evenly spaced points in the range
    step = (a - b) / 49
    points = [a + i * step for i in range(50)]
    
    # Find the maximum absolute difference
    z0 = max(abs(x - x0) for x in points)

    n = np.floor(np.log(z0 / epsilon) / np.log(1 / q)) + 1
    
    print(f"\nn >= {n}")

    print(f"\n{'Iteration':^10}{'x':^15}{'f(x)':^15}")
    print("-" * 42)

    while True:
        if iterations > 100:
            print("Перевищено максимальну кількість ітерацій.")
            return x, iterations

        x_next = x + sign * tao * f(x)
        iterations += 1

        if (iterations < 20):
            print(f"{iterations:^10}{x:^15.6f}{f(x):^15.6e}")
        
        if abs(x_next - x) < epsilon:
            return x_next, iterations
        
        x = x_next


# Перевірка зміни знаків функції на кінцях проміжку
print('Початок проміжку: ')
a = int(input())
print('Кінець проміжку: ')
b = int(input())

print("-" * 54)

if has_root(f, a, b):
    print("Зміна знаків функції на кінцях проміжку, корінь існує.")
else:
    print("Функція не має кореня на даному проміжку.")
    exit(1)

print("-" * 54)

# Виклик методу простої ітерації
root_simple_iteration, iterations_simple_iteration = simple_iteration_method(phi, dphi, a, b, 1e-4)
print("\nКорінь методом простої ітерації:", root_simple_iteration)
print("Кількість ітерацій:", iterations_simple_iteration)
print("-" * 54)

# Виклик методу релаксації
root_relaxation, iterations_relaxation = relaxation_method(f, df, a, b, 1e-4)
print("\nКорінь методом релаксації:", root_relaxation)
print("Кількість ітерацій:", iterations_relaxation)