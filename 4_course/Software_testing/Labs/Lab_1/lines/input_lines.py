from lines.constants import LOWER_BOUND, UPPER_BOUND
from lines.geometry import is_zero


def read_float(prompt: str) -> float:
    while True:
        raw = input(prompt).strip()
        try:
            return float(raw)
        except ValueError:
            print(f"  [Помилка] '{raw}' — не число. Введіть числове значення.")


def read_int_in_range(prompt: str) -> int:
    while True:
        raw = input(prompt).strip()
        try:
            val = int(raw)
        except ValueError:
            print(f"  [Помилка] '{raw}' — не ціле число. Спробуйте ще раз.")
            continue
        if val < LOWER_BOUND or val > UPPER_BOUND:
            print(
                f"  [Помилка] Значення {val} виходить за межі [{LOWER_BOUND}; {UPPER_BOUND}]. "
                f"Введіть число з цього проміжку."
            )
            continue
        return val


def input_line1() -> tuple:
    """Пряма 1 — канонічне рівняння (x-x0)/l = (y-y0)/m → (A, B, C)"""
    print("\n--- Пряма 1: канонічне рівняння (x-x0)/l = (y-y0)/m ---")
    while True:
        x0 = read_int_in_range("  Задайте x0: ")
        y0 = read_int_in_range("  Задайте y0: ")
        l  = read_int_in_range("  Задайте l (≠ 0): ")
        m  = read_int_in_range("  Задайте m (≠ 0): ")
        if is_zero(l) and is_zero(m):
            print("  [Помилка] l і m не можуть бути одночасно нулями.")
            continue
        if is_zero(l):
            print("  [Помилка] l = 0. Введіть l ≠ 0.")
            continue
        if is_zero(m):
            print("  [Помилка] m = 0. Введіть m ≠ 0.")
            continue
        return float(m), float(-l), float(-m * x0 + l * y0)


def input_line2() -> tuple:
    """Пряма 2 — рівняння у відрізках x/a + y/b = 1 → (A, B, C)"""
    print("\n--- Пряма 2: рівняння у відрізках x/a + y/b = 1 ---")
    while True:
        a = read_int_in_range("  Задайте a (відрізок на осі Ox, ≠ 0): ")
        b = read_int_in_range("  Задайте b (відрізок на осі Oy, ≠ 0): ")
        if is_zero(a):
            print("  [Помилка] a = 0. Введіть a ≠ 0.")
            continue
        if is_zero(b):
            print("  [Помилка] b = 0. Введіть b ≠ 0.")
            continue
        return float(b), float(a), float(-a * b)


def input_line3() -> tuple:
    """Пряма 3 — a(x-x0)+b(y-y0)=0 → (A, B, C)"""
    print("\n--- Пряма 3: рівняння через точку перпендикулярно вектору ---")
    while True:
        x0 = read_int_in_range("  Задайте x0: ")
        y0 = read_int_in_range("  Задайте y0: ")
        a  = read_int_in_range("  Задайте a: ")
        b  = read_int_in_range("  Задайте b: ")
        if is_zero(a) and is_zero(b):
            print("  [Помилка] a і b не можуть бути одночасно нулями.")
            continue
        return float(a), float(b), float(-a * x0 - b * y0)