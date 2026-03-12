"""
Метод Кронекера для розкладання многочленів на множники над ℤ[x].
Виводить усі проміжні кроки.
"""

from fractions import Fraction
from itertools import product as iproduct
import sys
sys.setrecursionlimit(300)


# ─────────────────────────── поліноміальна арифметика ────────────────────────

def poly_str(coeffs):
    n = len(coeffs) - 1
    terms = []
    for i, c in enumerate(coeffs):
        deg = n - i
        c = Fraction(c)
        if c == 0:
            continue
        abs_c = abs(c)
        sign  = "-" if c < 0 else "+"
        if deg == 0:
            s = str(abs_c)
        elif deg == 1:
            s = ("" if abs_c == 1 else str(abs_c)) + "x"
        else:
            s = ("" if abs_c == 1 else str(abs_c)) + f"x^{deg}"
        if not terms:
            terms.append(("-" if c < 0 else "") + s)
        else:
            terms.append(f" {sign} {s}")
    return "".join(terms) if terms else "0"


def poly_eval(coeffs, x):
    x = Fraction(x)
    r = Fraction(0)
    for c in coeffs:
        r = r * x + Fraction(c)
    return r


def poly_divmod(dividend, divisor):
    dividend = [Fraction(c) for c in dividend]
    divisor  = [Fraction(c) for c in divisor]
    if len(dividend) < len(divisor):
        return [Fraction(0)], dividend
    rem = list(dividend)
    quot = []
    for i in range(len(dividend) - len(divisor) + 1):
        coef = rem[i] / divisor[0]
        quot.append(coef)
        for j, d in enumerate(divisor):
            rem[i + j] -= coef * d
    rem = rem[len(dividend) - len(divisor) + 1:] or [Fraction(0)]
    return quot, rem


def is_zero(coeffs):
    return all(Fraction(c) == 0 for c in coeffs)


def normalize(coeffs):
    coeffs = [Fraction(c) for c in coeffs]
    while len(coeffs) > 1 and coeffs[0] == 0:
        coeffs.pop(0)
    return coeffs


def is_int_poly(coeffs):
    return all(Fraction(c).denominator == 1 for c in coeffs)


def make_primitive(coeffs):
    """Виносить НСД і повертає (content, primitive_poly).
    Примітивний многочлен має НСД коефіцієнтів = 1 і додатній старший коефіцієнт."""
    from math import gcd
    from functools import reduce
    coeffs = [Fraction(c) for c in coeffs]
    if not is_int_poly(coeffs):
        return Fraction(1), coeffs
    ints = [int(c) for c in coeffs]
    g = reduce(gcd, (abs(x) for x in ints if x != 0), 0)
    if g == 0:
        return Fraction(1), coeffs
    # знак = знак старшого коефіцієнта
    sign = 1 if ints[0] > 0 else -1
    content = Fraction(sign * g)
    prim = [Fraction(c) / content for c in coeffs]
    return content, prim


def int_divisors(n: int):
    n = abs(n)
    if n == 0:
        return [0]
    divs = []
    for i in range(1, n + 1):
        if n % i == 0:
            divs.extend([i, -i])
    return sorted(set(divs))


def poly_mul(a, b):
    res = [Fraction(0)] * (len(a) + len(b) - 1)
    for i, ca in enumerate(a):
        for j, cb in enumerate(b):
            res[i + j] += Fraction(ca) * Fraction(cb)
    return res


def lagrange_poly(xs, ys):
    n   = len(xs)
    xs  = [Fraction(x) for x in xs]
    ys  = [Fraction(y) for y in ys]
    res = [Fraction(0)] * n
    for i in range(n):
        basis = [Fraction(1)]
        denom = Fraction(1)
        for j in range(n):
            if i == j:
                continue
            denom *= xs[i] - xs[j]
            basis  = poly_mul(basis, [Fraction(1), -xs[j]])
        for k in range(n):
            res[k] += ys[i] * basis[k] / denom
    return normalize(res)


def rational_root_candidates(poly):
    a0 = abs(int(Fraction(poly[-1])))
    an = abs(int(Fraction(poly[0])))
    ps = int_divisors(a0) if a0 else [0]
    qs = [q for q in int_divisors(an) if q > 0]
    cands = set()
    for p in ps:
        for q in qs:
            cands.add(Fraction(p, q))
    return sorted(cands, key=lambda x: (abs(x), x < 0))


# ──────────────────────────── метод Кронекера ────────────────────────────────

_seen = set()


def kronecker_factor(poly, depth=0):
    indent = "  " * depth

    # Спочатку виносимо числовий НСД — робимо примітивним
    content, poly = make_primitive(poly)
    poly = normalize(poly)
    deg  = len(poly) - 1

    key = tuple(poly)
    if key in _seen:
        return [poly]
    _seen.add(key)

    print(f"\n{indent}{'═'*56}")
    print(f"{indent}Розкладаємо: {poly_str(poly)}  (ступінь {deg})")
    if content != 1:
        print(f"{indent}[після виносу числового множника {content}]")
    print(f"{indent}{'═'*56}")

    result_prefix = []
    if content != 1:
        result_prefix = [[content]]

    if deg == 0:
        print(f"{indent}→ Константа.")
        return result_prefix + [poly]
    if deg == 1:
        print(f"{indent}→ Лінійний — незвідний.")
        return result_prefix + [poly]

    # Квадратний: дискримінант
    if deg == 2:
        a, b, c = [Fraction(x) for x in poly]
        D = b*b - 4*a*c
        print(f"{indent}Дискримінант: D = {b}² - 4·{a}·{c} = {D}")
        if D < 0:
            print(f"{indent}→ D < 0 ⟹ незвідний над ℝ.")
            return result_prefix + [poly]

    # Метод Кронекера
    m      = deg // 2
    points = list(range(m + 1))

    print(f"\n{indent}Шукаємо множник ступеня ≤ {m}.  Вузли: {points}")

    values = []
    print(f"\n{indent}Значення P у вузлах:")
    for x in points:
        v = poly_eval(poly, x)
        values.append(v)
        print(f"{indent}  P({x}) = {v}")

    # P(0)=0 → x дільник
    if values[0] == 0:
        print(f"\n{indent}P(0)=0  ⟹  ділимо на x:")
        q, _ = poly_divmod(poly, [1, 0])
        q = normalize(q)
        print(f"{indent}  {poly_str(poly)} = x · ({poly_str(q)})")
        return result_prefix + \
               [[Fraction(1), Fraction(0)]] + \
               kronecker_factor(q, depth + 1)

    # Дільники
    div_sets = []
    bad = False
    print(f"\n{indent}Дільники значень:")
    for x, v in zip(points, values):
        if v.denominator != 1:
            print(f"{indent}  P({x})={v} — не ціле → переходимо до раціональних коренів.")
            bad = True
            break
        divs = int_divisors(int(v))
        div_sets.append(divs)
        print(f"{indent}  дільники P({x})={v}: {divs}")

    if bad:
        return result_prefix + _try_roots(poly, depth)

    # Перебір
    total = 1
    for d in div_sets:
        total *= len(d)
    print(f"\n{indent}Перебираємо {total} комбінацій (інтерполяція Лагранжа)...")

    found = None
    for combo in iproduct(*div_sets):
        g = lagrange_poly(points, list(combo))

        # g має бути з цілими коефіцієнтами та ненульовим старшим
        if not is_int_poly(g):
            continue
        if g[0] == 0:
            continue
        g_deg = len(g) - 1
        # Пропускаємо тривіальні константи ±1
        if g_deg == 0 and abs(g[0]) in [Fraction(0), Fraction(1)]:
            continue

        q, r = poly_divmod(poly, g)
        if not is_zero(r):
            continue

        q = normalize(q)
        # Перевіряємо, що q має цілі коефіцієнти
        if not is_int_poly(q):
            continue
        # Не тривіальний поділ
        if normalize(g) == normalize(poly) or normalize(q) == normalize([Fraction(1)]):
            continue

        print(f"\n{indent}  ✓ Множник: g(x) = {poly_str(g)}  (g(0..{m}) = {combo})")
        print(f"{indent}    {poly_str(poly)} = ({poly_str(g)}) · ({poly_str(q)})")
        found = (g, q)
        break

    if found is None:
        print(f"\n{indent}Множника не знайдено серед перевірених комбінацій.")
        return result_prefix + _try_roots(poly, depth)

    g, quotient = found
    return (result_prefix +
            kronecker_factor(normalize(g),       depth + 1) +
            kronecker_factor(normalize(quotient), depth + 1))


def _try_roots(poly, depth):
    indent = "  " * depth
    print(f"\n{indent}Шукаємо раціональні корені...")

    if not is_int_poly(poly):
        print(f"{indent}→ Нецілі коефіцієнти — незвідний.")
        return [normalize(poly)]

    cands = rational_root_candidates(poly)
    short = cands[:12]
    print(f"{indent}  Кандидати ({len(cands)}): {short}{'...' if len(cands)>12 else ''}")

    for r in cands:
        v = poly_eval(poly, r)
        if v == 0:
            g = normalize([Fraction(1), -r])
            q, _ = poly_divmod(poly, g)
            q = normalize(q)
            print(f"\n{indent}  ✓ Корінь x = {r}  ⟹  ({poly_str(g)})")
            print(f"{indent}    {poly_str(poly)} = ({poly_str(g)}) · ({poly_str(q)})")
            return (kronecker_factor(g, depth + 1) +
                    kronecker_factor(q, depth + 1))

    print(f"{indent}→ Незвідний над ℚ.")
    return [normalize(poly)]


# ─────────────────────────────── верхній рівень ──────────────────────────────

def factorize(coeffs, name="P"):
    _seen.clear()
    print(f"\n{'#'*65}")
    print(f"# {name}(x) = {poly_str(coeffs)}")
    print(f"{'#'*65}")

    factors = kronecker_factor(coeffs)
    factors = [normalize(f) for f in factors]

    # Збираємо константи і поліноми
    consts = [f[0] for f in factors if len(f) == 1]
    polys  = [f     for f in factors if len(f) >  1]

    const_prod = Fraction(1)
    for c in consts:
        const_prod *= c

    parts = []
    if const_prod != 1:
        parts.append(str(const_prod))
    for f in polys:
        parts.append(f"({poly_str(f)})")

    result = " · ".join(parts) if parts else "1"
    print(f"\n{'─'*65}")
    print(f"ВІДПОВІДЬ:  {name}(x) = {result}")
    print(f"{'─'*65}\n")


# ─────────────────────────────── тести ───────────────────────────────────────

if __name__ == "__main__":
    # P1 = 4x^4 - 31x^3 + 33x^2 - 93x + 63
    # Очікується: (x-7)(4x-3)(x^2+3)
    factorize([4, -31, 33, -93, 63], "P1")

    # P2 = x^4 + 7x^3 + 21x^2 + 63x + 108
    # Очікується: (x+3)(x+4)(x^2+9)
    factorize([1, 7, 21, 63, 108], "P2")