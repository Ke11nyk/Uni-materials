from lines.geometry import lines_coincide, lines_parallel, intersection, points_equal, fmt_point


def analyze_three_lines(line1, line2, line3):
    """Визначає взаємне розміщення трьох прямих і виводить результат."""
    A1, B1, C1 = line1
    A2, B2, C2 = line2
    A3, B3, C3 = line3

    c12 = lines_coincide(A1, B1, C1, A2, B2, C2)
    c13 = lines_coincide(A1, B1, C1, A3, B3, C3)
    c23 = lines_coincide(A2, B2, C2, A3, B3, C3)

    p12 = lines_parallel(A1, B1, C1, A2, B2, C2)
    p13 = lines_parallel(A1, B1, C1, A3, B3, C3)
    p23 = lines_parallel(A2, B2, C2, A3, B3, C3)

    # Випадок 1: всі три співпадають
    if c12 and c13:
        print("\nРезультат: Прямі співпадають")
        return

    # Випадок 2: не перетинаються
    all_parallel_or_coincide = (
        (c12 or p12) and (c13 or p13) and (c23 or p23)
    )
    if all_parallel_or_coincide:
        print("\nРезультат: Прямі не перетинаються")
        return

    # Точки перетину пар
    pt12 = intersection(A1, B1, C1, A2, B2, C2)
    pt13 = intersection(A1, B1, C1, A3, B3, C3)
    pt23 = intersection(A2, B2, C2, A3, B3, C3)

    # Випадок 3: єдина точка перетину (перевірка через pt12)
    if pt12 is not None:
        same_all = True
        if pt13 is not None and not points_equal(pt12, pt13):
            same_all = False
        if pt23 is not None and not points_equal(pt12, pt23):
            same_all = False

        if same_all:
            pt = pt12
            print(f"\nРезультат: Єдина точка перетину прямих (x0, y0), "
                  f"x0 = {fmt_point(pt)[1:-1].split(',')[0].strip()}, "
                  f"y0 = {fmt_point(pt)[1:-1].split(',')[1].strip()}")
            return

    # Підрахунок унікальних точок
    unique_points = []
    for pt in [pt12, pt13, pt23]:
        if pt is None:
            continue
        if not any(points_equal(pt, up) for up in unique_points):
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
        print("\nРезультат: Прямі не перетинаються")