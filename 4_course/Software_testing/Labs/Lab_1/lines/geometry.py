from .constants import EPSILON


def is_zero(z: float) -> bool:
    return abs(z) < EPSILON


def lines_coincide(A1, B1, C1, A2, B2, C2) -> bool:
    return (is_zero(A1 * B2 - A2 * B1) and
            is_zero(A1 * C2 - A2 * C1) and
            is_zero(B1 * C2 - B2 * C1))


def lines_parallel(A1, B1, C1, A2, B2, C2) -> bool:
    return is_zero(A1 * B2 - A2 * B1) and not lines_coincide(A1, B1, C1, A2, B2, C2)


def intersection(A1, B1, C1, A2, B2, C2):
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
    xi = round(x)
    yi = round(y)
    xs = str(xi) if is_zero(x - xi) else f"{x:.6g}"
    ys = str(yi) if is_zero(y - yi) else f"{y:.6g}"
    return f"({xs}, {ys})"