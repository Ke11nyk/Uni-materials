import numpy as np
import time
import os
import sys
import matplotlib.pyplot as plt

# Налаштування кодування для Windows
if sys.platform.startswith('win'):
    sys.stdin.reconfigure(encoding='utf-8')
    sys.stdout.reconfigure(encoding='utf-8')

sys.path.append(os.path.dirname(os.path.dirname(__file__)))
from Lab_2.NM_Lab_2_v2 import gaussian_elimination

def f(x, y):
    return np.array([
        np.sin(x - y) - x * y + 1,
        x**2 - y**2 - 0.75
    ])

def plot_system():
    x = np.linspace(-2, 2, 300)
    y = np.linspace(-2, 2, 300)
    X, Y = np.meshgrid(x, y)
    
    Z1 = np.sin(X - Y) - X * Y + 1
    Z2 = X**2 - Y**2 - 0.75
    
    plt.figure(figsize=(10, 8))
    
    plt.contour(X, Y, Z1, levels=[0], colors='blue', label='sin(x-y) - xy + 1 = 0')
    plt.contour(X, Y, Z2, levels=[0], colors='red', label='x² - y² = 0.75')
    
    plt.grid(True)
    plt.xlabel('x')
    plt.ylabel('y')
    plt.title('System of Equations')
    plt.legend()
    plt.axis('equal')
    
    plt.axhline(y=0, color='k', linestyle='--', alpha=0.3)
    plt.axvline(x=0, color='k', linestyle='--', alpha=0.3)
    
    plt.savefig('system_plot.png')
    plt.show()
    plt.close()

def jacobian(x, y):
    return np.array([
        [np.cos(x - y) - y, -np.cos(x - y) - x],
        [2*x, -2*y]
    ])

def is_jacobian_singular(J, tolerance=1e-10):
    """Перевіряє, чи є матриця Якобіана виродженою"""
    return abs(np.linalg.det(J)) < tolerance

def newton_method(x0, y0, epsilon, log_file, max_iterations=100):
    """
    Модифікований метод Ньютона з перевірками та обробкою помилок
    """
    x, y = x0, y0
    iteration = 0
    
    log_file.write(f"Initial approximation: x = {x}, y = {y}\n")
    log_file.write("Iteration,x,y,||F(x,y)||,det(J)\n")
    
    while iteration < max_iterations:
        # Обчислення Якобіана та перевірка на виродженість
        J = jacobian(x, y)
        det_J = np.linalg.det(J)
        
        if is_jacobian_singular(J):
            message = f"Попередження: Якобіан вироджений в точці ({x}, {y}). Детермінант = {det_J}"
            print(message)
            log_file.write(f"\n{message}\n")
            return None, None
        
        F = f(x, y)
        norm_F = np.linalg.norm(F)
        
        # Запис у лог
        log_file.write(f"{iteration},{x},{y},{norm_F},{det_J}\n")
        
        # Розв'язання системи
        try:
            z = gaussian_elimination(J, -F)  # Додано мінус перед F для правильного напрямку
            if z is None:
                raise ValueError("Gaussian elimination failed")
        except Exception as e:
            message = f"Помилка при розв'язанні системи на ітерації {iteration}: {str(e)}"
            print(message)
            log_file.write(f"\n{message}\n")
            return None, None
        
        # Оновлення наближення
        x_new = x + z[0]
        y_new = y + z[1]
        
        # Перевірка збіжності
        if np.linalg.norm(z) < epsilon:
            log_file.write(f"\nРозв'язок знайдено на ітерації {iteration}\n")
            return x_new, y_new
        
        x, y = x_new, y_new
        iteration += 1
    
    message = f"Метод не збігся за {max_iterations} ітерацій"
    print(message)
    log_file.write(f"\n{message}\n")
    return None, None

def analyze_singular_points(x_range=(-2, 2), y_range=(-2, 2), points_count=200):
    """Аналізує область на наявність особливих точок"""
    x = np.linspace(x_range[0], x_range[1], points_count)
    y = np.linspace(y_range[0], y_range[1], points_count)
    X, Y = np.meshgrid(x, y)
    
    # Обчислення детермінанта Якобіана
    det_J = 2*(X**2 + Y**2 + (X-Y)*np.cos(X-Y))
    
    # Створення графіку
    plt.figure(figsize=(12, 8))
    
    # Контурний графік детермінанта
    plt.contour(X, Y, det_J, levels=[0], colors='red', linewidths=2, label='det(J) = 0')
    
    # Додавання графіків початкової системи
    Z1 = np.sin(X - Y) - X * Y + 1
    Z2 = X**2 - Y**2 - 0.75
    plt.contour(X, Y, Z1, levels=[0], colors='blue', label='sin(x-y) - xy + 1 = 0')
    plt.contour(X, Y, Z2, levels=[0], colors='green', label='x² - y² = 0.75')
    
    # Позначення точки (0,0)
    plt.plot(0, 0, 'ro', markersize=10, label='(0,0)')
    
    # Знаходження інших точок, де детермінант близький до нуля
    singular_points = []
    threshold = 1e-3
    for i in range(len(x)):
        for j in range(len(y)):
            if abs(det_J[i,j]) < threshold:
                if abs(x[i]) > threshold or abs(y[j]) > threshold:  # Виключаємо точку (0,0)
                    # Перевіряємо, чи точка також близька до обох кривих
                    if abs(Z1[i,j]) < threshold and abs(Z2[i,j]) < threshold:
                        singular_points.append((x[i], y[j]))
                        plt.plot(x[i], y[j], 'ro', markersize=10)
    
    plt.grid(True)
    plt.xlabel('x')
    plt.ylabel('y')
    plt.title('Особливі точки системи\n(червоні точки - особливі точки, червоні криві - det(J) = 0)')
    plt.legend()
    plt.axis('equal')
    
    plt.savefig('singular_points.png')
    plt.show()
    plt.close()
    
    return singular_points

def suggest_initial_guess():
    """Пропонує початкові наближення, уникаючи особливих точок"""
    suggestions = [
        (1.0, 0.5),    # Наближення до першого розв'язку
        (-1.0, -0.5),  # Наближення до другого розв'язку
        (0.5, -0.2),   # Безпечна точка
    ]
    return suggestions

def main():
    print("Розв'язання системи рівнянь методом Ньютона:")
    print("sin(x - y) - x * y = -1")
    print("x^2 - y^2 = 0.75")

    print("\nАналіз особливих точок системи...")
    singular_points = analyze_singular_points()
    print("\nЗнайдені особливі точки (крім (0,0)):")
    for x, y in singular_points:
        print(f"x ≈ {x:.4f}, y ≈ {y:.4f}")
    
    print("\nСтворення графіку системи рівнянь...")
    plot_system()
    print("Графік збережено у файлі: system_plot.png")
    
    while True:
        try:
            x0 = float(input("\nВведіть початкове наближення для x: "))
            y0 = float(input("Введіть початкове наближення для y: "))
            
            if x0 == 0 and y0 == 0:
                print("\nУвага: точка (0,0) є особливою для цієї системи!")
                print("Рекомендовані початкові наближення:")
                for i, (x, y) in enumerate(suggest_initial_guess(), 1):
                    print(f"{i}. (x, y) = ({x}, {y})")
                
                # Змінений блок вводу
                while True:
                    choice = input("\nБажаєте спробувати інше наближення? (1 - так, 0 - ні): ").strip()
                    if choice in ('0', '1'):
                        if choice == '1':
                            break  # Повертаємось до початку зовнішнього циклу
                        return  # Завершуємо програму, якщо користувач не хоче продовжувати
                    print("Будь ласка, введіть 1 (так) або 0 (ні)")
                continue
            
            epsilon = float(input("Введіть точність (епсилон): "))
            break
        except ValueError:
            print("Некоректний ввід. Спробуйте ще раз.")
    
    log_filename = "newton_method_log.csv"
    with open(log_filename, "w", encoding='utf-8') as log_file:
        start_time = time.time()
        x, y = newton_method(x0, y0, epsilon, log_file)
        end_time = time.time()
    
    if x is not None and y is not None:
        print(f"\nРозв'язок:")
        print(f"x = {x}")
        print(f"y = {y}")
        print(f"\nЧас виконання: {end_time - start_time:.6f} секунд")
        
        F = f(x, y)
        print(f"\nПеревірка (має бути близько до нуля):")
        print(f"f1(x,y) = {F[0]}")
        print(f"f2(x,y) = {F[1]}")
    else:
        print("\nНе вдалося знайти розв'язок. Спробуйте інше початкове наближення.")
    
    print(f"\nЛог ітерацій збережено у файлі: {os.path.abspath(log_filename)}")

if __name__ == "__main__":
    main()