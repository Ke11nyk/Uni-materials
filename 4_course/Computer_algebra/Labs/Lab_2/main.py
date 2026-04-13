import time
from sympy import symbols, Poly, QQ, expand, groebner

x, y, z = symbols('x y z')
GENS = (x, y, z)   # порядок змінних: x > y > z (lex)

def P(expr):
    """Створити многочлен у QQ[x,y,z]."""
    return Poly(expand(expr), *GENS, domain=QQ)

# Початкова система №8
f1 = P(x*y - x*z + y**2)
f2 = P(y*z - x**2 + x**2*y)
f3 = P(x - x*y + y)


def monomial_to_expr(m):
    """Перетворити кортеж степенів у моном."""
    result = 1
    for var, power in zip(GENS, m):
        result *= var**power
    return result

def monomial_divides(a, b):
    """
    Перевірка: чи ділить моном x^a моном x^b.
    a, b - кортежі степенів.
    """
    return all(ai <= bi for ai, bi in zip(a, b))

def monomial_quotient(num, den):
    """num / den для мономів, якщо den | num."""
    return tuple(ni - di for ni, di in zip(num, den))

def monomial_lcm(a, b):
    """НСК двох мономів."""
    return tuple(max(ai, bi) for ai, bi in zip(a, b))


def LM(p):
    """Leading monomial."""
    mons = p.monoms()
    return None if not mons else mons[0]

def LC(p):
    """Leading coefficient."""
    coeffs = p.coeffs()
    return 0 if not coeffs else coeffs[0]

def LT_expr(p):
    """Leading term як вираз."""
    if p.as_expr() == 0:
        return 0
    return LC(p) * monomial_to_expr(LM(p))



def s_polynomial(p, q):
    """
    S(p,q) = lcm(LM(p),LM(q))/LT(p)*p - lcm(LM(p),LM(q))/LT(q)*q
    """
    mp = LM(p)
    mq = LM(q)
    l = monomial_lcm(mp, mq)

    mult_p = monomial_to_expr(monomial_quotient(l, mp))
    mult_q = monomial_to_expr(monomial_quotient(l, mq))

    sp = mult_p / LC(p) * p.as_expr() - mult_q / LC(q) * q.as_expr()
    return P(sp)


def reduce_poly(f, G, trace=False):
    """
    Звести f за списком G.
    Повертає:
      remainder, steps
    де remainder - остача,
       steps - кроки редукції.
    """
    p = P(f.as_expr() if isinstance(f, Poly) else f)
    r = P(0)
    steps = []

    while p.as_expr() != 0:
        reduced = False
        mp = LM(p)
        cp = LC(p)

        for g in G:
            if g.as_expr() == 0:
                continue

            mg = LM(g)
            cg = LC(g)

            if monomial_divides(mg, mp):
                qmon = monomial_quotient(mp, mg)
                factor = cp / cg * monomial_to_expr(qmon)
                new_p = P(p.as_expr() - factor * g.as_expr())

                if trace:
                    steps.append({
                        "type": "reduce",
                        "current": p.as_expr(),
                        "by": g.as_expr(),
                        "factor": factor,
                        "result": new_p.as_expr()
                    })

                p = new_p
                reduced = True
                break

        if not reduced:
            lt = LT_expr(p)
            r = P(r.as_expr() + lt)
            new_p = P(p.as_expr() - lt)

            if trace:
                steps.append({
                    "type": "move_to_remainder",
                    "current": p.as_expr(),
                    "lt": lt,
                    "new_current": new_p.as_expr(),
                    "remainder": r.as_expr()
                })

            p = new_p

    return r, steps


def buchberger(F):
    """
    Побудова базису Гребнера алгоритмом Бухбергера.
    Повертає:
      G, history
    де G - отриманий базис,
       history - інформація по S-поліномах.
    """
    G = [P(f.as_expr() if isinstance(f, Poly) else f) for f in F]
    #pairs = [(i, j) for i in range(len(G)) for j in range(i)]
    pairs = [(2, 0), (2, 1), (1, 0)] # altering the range to correspond to the report
    history = []

    while pairs:
        i, j = pairs.pop(0)

        S = s_polynomial(G[i], G[j])
        R, steps = reduce_poly(S, G, trace=True)

        history.append({
            "pair": (i, j),
            "S": S.as_expr(),
            "R": R.as_expr(),
            "added": (R.as_expr() != 0),
            "steps": steps
        })

        if R.as_expr() != 0:
            G.append(R)
            new_idx = len(G) - 1
            for k in range(new_idx):
                pairs.append((new_idx, k))

    return G, history


def make_monic(p):
    """Зробити поліном унітарним."""
    if p.as_expr() == 0:
        return p
    return P(p.as_expr() / LC(p))

def minimal_groebner_basis(G):
    """
    Побудова мінімального базису:
    1) робимо поліноми унітарними,
    2) викидаємо ті, чий LM ділиться іншим LM.
    """
    Gm = [make_monic(g) for g in G if g.as_expr() != 0]

    result = []
    for i, gi in enumerate(Gm):
        keep = True
        for j, gj in enumerate(Gm):
            if i != j and monomial_divides(LM(gj), LM(gi)):
                # якщо LM(gj) | LM(gi), то gi зайвий
                keep = False
                break
        if keep:
            result.append(gi)

    # приберемо дублікати
    unique = []
    seen = set()
    for g in result:
        expr = expand(g.as_expr())
        if expr not in seen:
            seen.add(expr)
            unique.append(P(expr))

    return unique


def reduced_groebner_basis(G):
    """
    Із мінімального базису отримати редукований:
    кожен поліном зводимо за іншими.
    """
    Gmin = minimal_groebner_basis(G)
    Gred = []

    for i, g in enumerate(Gmin):
        others = [Gmin[j] for j in range(len(Gmin)) if j != i]
        r, _ = reduce_poly(g, others, trace=False)
        Gred.append(make_monic(r))

    # прибрати нулі та дублікати
    final = []
    seen = set()
    for g in Gred:
        if g.as_expr() != 0:
            expr = expand(g.as_expr())
            if expr not in seen:
                seen.add(expr)
                final.append(P(expr))

    return final


if __name__ == "__main__":
    F = [f1, f2, f3]

    print("Початкова система:")
    for i, f in enumerate(F, 1):
        print(f"f{i} =", f.as_expr())

    # власна реалізація
    start = time.perf_counter()

    G, history = buchberger(F)

    print("\n=== Кроки Бухбергера ===")
    for k, item in enumerate(history, 1):
        i, j = item["pair"]
        a, b = sorted((i + 1, j + 1))
        print(f"Крок {k}: S(f{a}, f{b})")

        mp = LM(G[i])
        mq = LM(G[j])
        lcm = monomial_lcm(mp, mq)

        print("Старші мономи:")
        print(f"LM = {monomial_to_expr(mp)}, {monomial_to_expr(mq)}")
        print(f"НСК = {monomial_to_expr(lcm)}")

        print("S-поліном: =", item["S"])
        print("Після редукції R:")
        print(item["R"])

        if item["added"]:
            print("=> Додаємо новий поліном до базису")
        else:
            print("=> Остача 0, новий поліном не додається")

    print("\n=== Отриманий базис Гребнера ===")
    for i, g in enumerate(G, 1):
        print(f"g{i} =", g.as_expr())

    Gmin = minimal_groebner_basis(G)
    print("\n=== Мінімальний базис Гребнера ===")
    for i, g in enumerate(Gmin, 1):
        print(f"m{i} =", g.as_expr())

    Gred = reduced_groebner_basis(G)

    end = time.perf_counter()
    custom_time = end - start

    print("\n=== ВЛАСНА РЕАЛІЗАЦІЯ ===")
    print(f"Час виконання: {custom_time:.6f} секунд")

    print("\nРедукований базис Гребнера:")
    for i, g in enumerate(Gred, 1):
        print(f"r{i} =", g.as_expr())

    # sympy method
    start = time.perf_counter()

    G_lib = groebner(
        [f1.as_expr(), f2.as_expr(), f3.as_expr()],
        x, y, z,
        order="lex"
    )

    end = time.perf_counter()
    lib_time = end - start

    print("\n=== БІБЛІОТЕЧНИЙ МЕТОД (SymPy) ===")
    print(f"Час виконання: {lib_time:.6f} секунд")
    print(G_lib)

    print("\n=== ПОРІВНЯННЯ ЧАСУ ===")
    print(f"Власна реалізація: {custom_time:.6f} с")
    print(f"Бібліотечний метод: {lib_time:.6f} с")

    if lib_time > 0:
        print(f"Прискорення бібліотеки: {custom_time / lib_time:.2f}x")