"""
Лабораторна робота №1 — Варіант 14
Визначення взаємного розміщення трьох прямих на площині.

Подання прямих:
  Пряма 1 — рівняння (3): канонічне (x-x0)/l = (y-y0)/m
  Пряма 2 — рівняння (4): у відрізках x/a + y/b = 1
  Пряма 3 — рівняння (6): через точку, перпендикулярно вектору a(x-x0)+b(y-y0)=0

Зведення до загального вигляду Ax + By + C = 0:
  Пряма 1: A1=m,  B1=-l,  C1=-m*x0+l*y0
  Пряма 2: A2=b,  B2=a,   C2=-a*b
  Пряма 3: A3=a,  B3=b,   C3=-a*x0-b*y0
"""

# ──────────────────────────────────────────────────────────
# Константи
# ──────────────────────────────────────────────────────────
LOWER_BOUND = -114
UPPER_BOUND = 114
EPSILON = 1e-8   # вважати |z| < EPSILON → z == 0


# ──────────────────────────────────────────────────────────
# Допоміжні функції
# ──────────────────────────────────────────────────────────

def is_zero(z: float) -> bool:
    return abs(z) < EPSILON


def read_float(prompt: str) -> float:
    """Зчитати дійсне число з консолі з перевіркою формату."""
    while True:
        raw = input(prompt).strip()
        try:
            return float(raw)
        except ValueError:
            print(f"  [Помилка] '{raw}' — не число. Введіть числове значення.")


def read_int_in_range(prompt: str) -> int:
    """Зчитати ціле число з проміжку [LOWER_BOUND; UPPER_BOUND]."""
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


# ──────────────────────────────────────────────────────────
# Введення прямих
# ──────────────────────────────────────────────────────────

def input_line1() -> tuple:
    """
    Пряма 1 — канонічне рівняння (3): (x-x0)/l = (y-y0)/m, l,m ≠ 0
    Повертає (A, B, C) загального вигляду:
      A = m,  B = -l,  C = -m*x0 + l*y0
    """
    print("\n--- Пряма 1: канонічне рівняння (x-x0)/l = (y-y0)/m ---")
    while True:
        x0 = read_int_in_range("  Задайте x0: ")
        y0 = read_int_in_range("  Задайте y0: ")
        l  = read_int_in_range("  Задайте l (≠ 0): ")
        m  = read_int_in_range("  Задайте m (≠ 0): ")
        if is_zero(l) and is_zero(m):
            print("  [Помилка] l і m не можуть бути одночасно нулями. Введіть знову.")
            continue
        if is_zero(l):
            print("  [Помилка] l = 0: напрямний вектор не може мати l=0 (пряма (3)). "
                  "Введіть l ≠ 0.")
            continue
        if is_zero(m):
            print("  [Помилка] m = 0: напрямний вектор не може мати m=0 (пряма (3)). "
                  "Введіть m ≠ 0.")
            continue
        A = float(m)
        B = float(-l)
        C = float(-m * x0 + l * y0)
        return A, B, C


def input_line2() -> tuple:
    """
    Пряма 2 — рівняння у відрізках (4): x/a + y/b = 1, a,b ≠ 0
    Загальний вигляд: b*x + a*y - a*b = 0
      A = b,  B = a,  C = -a*b
    """
    print("\n--- Пряма 2: рівняння у відрізках x/a + y/b = 1 ---")
    while True:
        a = read_int_in_range("  Задайте a (відрізок на осі Ox, ≠ 0): ")
        b = read_int_in_range("  Задайте b (відрізок на осі Oy, ≠ 0): ")
        if is_zero(a):
            print("  [Помилка] a = 0: відрізок на осі Ox не може дорівнювати нулю. "
                  "Введіть a ≠ 0.")
            continue
        if is_zero(b):
            print("  [Помилка] b = 0: відрізок на осі Oy не може дорівнювати нулю. "
                  "Введіть b ≠ 0.")
            continue
        A = float(b)
        B = float(a)
        C = float(-a * b)
        return A, B, C


def input_line3() -> tuple:
    """
    Пряма 3 — рівняння (6): a(x-x0) + b(y-y0) = 0, a²+b² ≠ 0
    Загальний вигляд: a*x + b*y - a*x0 - b*y0 = 0
      A = a,  B = b,  C = -a*x0 - b*y0
    """
    print("\n--- Пряма 3: рівняння через точку перпендикулярно вектору a(x-x0)+b(y-y0)=0 ---")
    while True:
        x0 = read_int_in_range("  Задайте x0: ")
        y0 = read_int_in_range("  Задайте y0: ")
        a  = read_int_in_range("  Задайте a (компонента нормального вектора): ")
        b  = read_int_in_range("  Задайте b (компонента нормального вектора): ")
        if is_zero(a) and is_zero(b):
            print("  [Помилка] a і b не можуть бути одночасно нулями (нульовий вектор). "
                  "Введіть знову.")
            continue
        A = float(a)
        B = float(b)
        C = float(-a * x0 - b * y0)
        return A, B, C


# ──────────────────────────────────────────────────────────
# Геометричні операції з парами прямих
# ──────────────────────────────────────────────────────────

def lines_coincide(A1, B1, C1, A2, B2, C2) -> bool:
    """Перевірити, чи дві прямі співпадають."""
    # A1/A2 = B1/B2 = C1/C2  (крос-множення, щоб уникнути ділення на 0)
    return (is_zero(A1 * B2 - A2 * B1) and
            is_zero(A1 * C2 - A2 * C1) and
            is_zero(B1 * C2 - B2 * C1))


def lines_parallel(A1, B1, C1, A2, B2, C2) -> bool:
    """Перевірити, чи дві прямі паралельні (але не співпадають)."""
    return is_zero(A1 * B2 - A2 * B1) and not lines_coincide(A1, B1, C1, A2, B2, C2)


def intersection(A1, B1, C1, A2, B2, C2):
    """
    Знайти точку перетину двох прямих методом Крамера.
    Повертає (x, y) або None, якщо паралельні/співпадають.
    """
    det = A1 * B2 - A2 * B1
    if is_zero(det):
        return None
    x = -(C1 * B2 - C2 * B1) / det
    y = -(A1 * C2 - A2 * C1) / det
    return x, y


def points_equal(p1, p2) -> bool:
    if p1 is None or p2 is None:
        return False
    return is_zero(p1[0] - p2[0]) and is_zero(p1[1] - p2[1])


def fmt_point(p) -> str:
    x, y = p
    # Якщо значення близьке до цілого — виводити як ціле
    xi = round(x)
    yi = round(y)
    xs = str(xi) if is_zero(x - xi) else f"{x:.6g}"
    ys = str(yi) if is_zero(y - yi) else f"{y:.6g}"
    return f"({xs}, {ys})"


# ──────────────────────────────────────────────────────────
# Основна логіка: визначення взаємного розміщення
# ──────────────────────────────────────────────────────────

def analyze_three_lines(line1, line2, line3):
    """
    Визначає взаємне розміщення трьох прямих і виводить результат.
    Кожна пряма задана кортежем (A, B, C).
    """
    A1, B1, C1 = line1
    A2, B2, C2 = line2
    A3, B3, C3 = line3

    c12 = lines_coincide(A1, B1, C1, A2, B2, C2)
    c13 = lines_coincide(A1, B1, C1, A3, B3, C3)
    c23 = lines_coincide(A2, B2, C2, A3, B3, C3)

    p12 = lines_parallel(A1, B1, C1, A2, B2, C2)
    p13 = lines_parallel(A1, B1, C1, A3, B3, C3)
    p23 = lines_parallel(A2, B2, C2, A3, B3, C3)

    # ── Випадок 1: всі три співпадають ────────────────────
    if c12 and c13:
        print("\nРезультат: Прямі співпадають")
        return

    # ── Випадок 2: не перетинаються ───────────────────────
    # Підвипадки:
    #   а) всі паралельні (жодна не співпадає)
    #   б) дві паралельні, третя паралельна до них
    #   в) дві співпадають, третя паралельна до них
    all_parallel_or_coincide = (
        (c12 or p12) and (c13 or p13) and (c23 or p23)
    )
    # Перевіряємо, що не всі три співпадають (вже оброблено вище)
    if all_parallel_or_coincide:
        print("\nРезультат: Прямі не перетинаються")
        return

    # ── Точки перетину пар ────────────────────────────────
    pt12 = intersection(A1, B1, C1, A2, B2, C2)
    pt13 = intersection(A1, B1, C1, A3, B3, C3)
    pt23 = intersection(A2, B2, C2, A3, B3, C3)

    # ── Випадок 3: єдина точка перетину ───────────────────
    # Якщо пара (1,2) дає точку, і (1,3) або (2,3) дають ту саму точку
    if pt12 is not None:
        # Перевіряємо, чи всі перетини в одній точці
        same_all = True
        if pt13 is not None and not points_equal(pt12, pt13):
            same_all = False
        if pt23 is not None and not points_equal(pt12, pt23):
            same_all = False

        if same_all:
            # Перевірка спеціального випадку: дві прямі співпадають,
            # третя перетинає їх в одній точці
            if c12 or c13 or c23:
                # Якщо дві співпадають, реальна точка перетину одна
                pt = pt12 or pt13 or pt23
            else:
                pt = pt12
            print(f"\nРезультат: Єдина точка перетину прямих (x0, y0), "
                  f"x0 = {fmt_point(pt)[1:-1].split(',')[0].strip()}, "
                  f"y0 = {fmt_point(pt)[1:-1].split(',')[1].strip()}")
            return

    # ── Підрахунок унікальних точок перетину ──────────────
    unique_points = []

    for pt in [pt12, pt13, pt23]:
        if pt is None:
            continue
        already = any(points_equal(pt, up) for up in unique_points)
        if not already:
            unique_points.append(pt)

    n = len(unique_points)

    if n == 1:
        print(f"\nРезультат: Єдина точка перетину прямих "
              f"x0 = {unique_points[0][0]:.6g}, y0 = {unique_points[0][1]:.6g}")
    elif n == 2:
        p1, p2 = unique_points
        print(f"\nРезультат: Дві точки перетину прямих "
              f"(x1, y1) = {fmt_point(p1)}, (x2, y2) = {fmt_point(p2)}")
    elif n == 3:
        p1, p2, p3 = unique_points
        print(f"\nРезультат: Три точки перетину прямих "
              f"(x1, y1) = {fmt_point(p1)}, "
              f"(x2, y2) = {fmt_point(p2)}, "
              f"(x3, y3) = {fmt_point(p3)}")
    else:
        # n == 0: всі пари або співпадають або паралельні (вже оброблено)
        print("\nРезультат: Прямі не перетинаються")


# ──────────────────────────────────────────────────────────
# ──────────────────────────────────────────────────────────

def main():
    print("=" * 60)
    print("Лабораторна робота №1 — Варіант 14")
    print("Взаємне розміщення трьох прямих на площині")
    print(f"Допустимий діапазон параметрів: [{LOWER_BOUND}; {UPPER_BOUND}]")
    print("=" * 60)

    while True:
        try:
            line1 = input_line1()
            line2 = input_line2()
            line3 = input_line3()

            # Виводимо загальні рівняння для контролю
            print("\n--- Загальні рівняння прямих (Ax + By + C = 0) ---")
            for i, (A, B, C) in enumerate([line1, line2, line3], 1):
                print(f"  Пряма {i}: {A:g}x + {B:g}y + {C:g} = 0")

            analyze_three_lines(line1, line2, line3)

        except KeyboardInterrupt:
            print("\n\nРоботу програми завершено.")
            break
        except Exception as e:
            print(f"\n[Критична помилка] {e}")
            print("Опис дій: перевірте введені дані та запустіть програму знову.\n")

        print()
        again = input("\nПовторити? (т/н): ").strip().lower()
        if again not in ("т", "y", "yes", "так", "t"):
            break


if __name__ == "__main__":
    main()