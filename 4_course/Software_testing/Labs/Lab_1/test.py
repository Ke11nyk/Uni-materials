import sys
import io
import unittest

# Додаємо шлях до main.py
sys.path.insert(0, '/mnt/user-data/outputs')
from main import (
    is_zero, lines_coincide, lines_parallel, intersection,
    points_equal, fmt_point, analyze_three_lines,
    LOWER_BOUND, UPPER_BOUND, EPSILON
)

# ─── Хелпери для побудови прямих із вхідних параметрів ───

def line1(x0, l, y0, m):
    """Пряма 1 (рівняння 3) → (A, B, C): A=m, B=-l, C=-m*x0+l*y0"""
    return float(m), float(-l), float(-m*x0 + l*y0)

def line2(a, b):
    """Пряма 2 (рівняння 4) → (A, B, C): A=b, B=a, C=-a*b"""
    return float(b), float(a), float(-a*b)

def line3(x0, y0, a, b):
    """Пряма 3 (рівняння 6) → (A, B, C): A=a, B=b, C=-a*x0-b*y0"""
    return float(a), float(b), float(-a*x0 - b*y0)

def get_result(L1, L2, L3) -> str:
    """Запускає analyze_three_lines і повертає рядок виводу."""
    buf = io.StringIO()
    old_stdout = sys.stdout
    sys.stdout = buf
    analyze_three_lines(L1, L2, L3)
    sys.stdout = old_stdout
    return buf.getvalue().strip()

def one_point_coords(output: str):
    """Витягує x0,y0 з рядка 'Єдина точка перетину...'"""
    import re
    m = re.search(r'x0\s*=\s*([^\s,]+).*?y0\s*=\s*([^\s,]+)', output)
    if m:
        return float(m.group(1)), float(m.group(2))
    return None


# ═══════════════════════════════════════════════════════════
class TestIsZero(unittest.TestCase):
    def test_zero(self):       self.assertTrue(is_zero(0.0))
    def test_near_zero(self):  self.assertTrue(is_zero(1e-9))
    def test_epsilon_border(self): self.assertFalse(is_zero(1e-7))
    def test_positive(self):   self.assertFalse(is_zero(1.0))
    def test_negative(self):   self.assertFalse(is_zero(-1.0))


class TestLinesCoincide(unittest.TestCase):
    def test_identical(self):
        L = (1.0, -1.0, 0.0)
        self.assertTrue(lines_coincide(*L, *L))

    def test_scaled(self):
        # 2x - 2y = 0  ≡  x - y = 0
        self.assertTrue(lines_coincide(1, -1, 0, 2, -2, 0))

    def test_parallel_not_coincide(self):
        self.assertFalse(lines_coincide(1, -1, 0, 1, -1, 5))

    def test_different(self):
        self.assertFalse(lines_coincide(1, 0, 0, 0, 1, 0))


class TestLinesParallel(unittest.TestCase):
    def test_parallel(self):
        # y=x  та  y=x+5
        L1 = line1(0, 1, 0, 1)       # A=1,B=-1,C=0
        L2 = line2(-1, 1)             # A=1,B=-1,C=1
        self.assertTrue(lines_parallel(*L1, *L2))

    def test_coincide_not_parallel(self):
        L = line1(0, 1, 0, 1)
        self.assertFalse(lines_parallel(*L, *L))

    def test_intersecting_not_parallel(self):
        L1 = line1(0, 1, 0, 1)       # y=x
        L2 = line2(2, 2)             # x+y=2
        self.assertFalse(lines_parallel(*L1, *L2))


class TestIntersection(unittest.TestCase):
    def test_basic(self):
        # y=x та x+y=2 → (1,1)
        L1 = line1(0, 1, 0, 1)
        L2 = line2(2, 2)
        pt = intersection(*L1, *L2)
        self.assertIsNotNone(pt)
        self.assertAlmostEqual(pt[0], 1.0)
        self.assertAlmostEqual(pt[1], 1.0)

    def test_parallel_returns_none(self):
        L1 = line1(0, 2, 0, 1)
        L2 = line2(-2, 1)
        self.assertIsNone(intersection(*L1, *L2))

    def test_coincide_returns_none(self):
        L = line1(0, 1, 0, 1)
        self.assertIsNone(intersection(*L, *L))

    def test_axes(self):
        # x=0 (ось Oy) та y=0 (ось Ox) → (0,0)
        L1 = (1.0, 0.0, 0.0)   # x=0
        L2 = (0.0, 1.0, 0.0)   # y=0
        pt = intersection(*L1, *L2)
        self.assertAlmostEqual(pt[0], 0.0)
        self.assertAlmostEqual(pt[1], 0.0)


class TestPointsEqual(unittest.TestCase):
    def test_equal(self):   self.assertTrue(points_equal((1.0, 2.0), (1.0, 2.0)))
    def test_near(self):    self.assertTrue(points_equal((1.0, 2.0), (1.0+1e-9, 2.0)))
    def test_not_equal(self): self.assertFalse(points_equal((1.0, 2.0), (1.0, 3.0)))
    def test_none(self):    self.assertFalse(points_equal(None, (1.0, 2.0)))


class TestFmtPoint(unittest.TestCase):
    def test_integers(self):
        self.assertEqual(fmt_point((1.0, 2.0)), "(1, 2)")

    def test_floats(self):
        result = fmt_point((1.5, -0.5))
        self.assertIn("1.5", result)
        self.assertIn("-0.5", result)

    def test_negative_integers(self):
        self.assertEqual(fmt_point((-57.0, -57.0)), "(-57, -57)")


# ═══════════════════════════════════════════════════════════
# Тести analyze_three_lines через перехоплення stdout
# ═══════════════════════════════════════════════════════════

class TestAnalyzeNoIntersection(unittest.TestCase):
    """Клас 2: Прямі не перетинаються"""

    # Базові паралельні: A=1,B=-2,C=0/2/-2
    L1p = line1(0, 2, 0, 1)
    L2p = line2(-2, 1)
    L3p = line3(2, 0, 1, -2)

    def _check(self, L1, L2, L3):
        out = get_result(L1, L2, L3)
        self.assertIn("не перетинаються", out)

    def test_1_left_bound(self):
        self._check(line1(-114,2,-114,1), line2(-2,1), line3(-114,-114,1,-2))

    def test_2_right_bound(self):
        self._check(line1(114,2,114,1), line2(-2,1), line3(114,114,1,-2))

    def test_3_middle(self):
        self._check(self.L1p, self.L2p, self.L3p)

    def test_4_left_mid_right(self):
        self._check(line1(-114,2,0,1), self.L2p, line3(114,0,1,-2))

    def test_5_two_near_left(self):
        self._check(line1(-113,2,-113,1), line2(-2,1), self.L3p)

    def test_6_two_near_right(self):
        self._check(line1(113,2,113,1), line2(-2,1), self.L3p)


class TestAnalyzeOnePoint(unittest.TestCase):
    """Клас 3: Єдина точка перетину"""

    def _check_point(self, L1, L2, L3, ex, ey):
        out = get_result(L1, L2, L3)
        self.assertIn("Єдина точка", out)
        coords = one_point_coords(out)
        self.assertIsNotNone(coords, f"Не знайдено координати у: {out}")
        self.assertAlmostEqual(coords[0], ex, places=4)
        self.assertAlmostEqual(coords[1], ey, places=4)

    def test_1_left_bound(self):
        # Три прямі через (-57,-57)
        L1 = line1(-57, 1, -57, 1)
        L2 = line2(-114, -114)
        L3 = line3(0, -114, 1, 1)
        self._check_point(L1, L2, L3, -57, -57)

    def test_2_right_bound(self):
        # Три прямі через (57,57)
        L1 = line1(57, 1, 57, 1)
        L2 = line2(114, 114)
        L3 = line3(57, 57, 2, 1)
        self._check_point(L1, L2, L3, 57, 57)

    def test_3_middle(self):
        # Три прямі через (1,1)
        L1 = line1(0, 1, 0, 1)
        L2 = line2(2, 2)
        L3 = line3(0, 0, 1, -1)
        self._check_point(L1, L2, L3, 1, 1)

    def test_4_left_mid_right(self):
        # L1: x-y=-114, L2: x+y=2, перетин: (-56,58)
        L1 = line1(-114, 1, 0, 1)
        L2 = line2(2, 2)
        L3 = line3(-56, 58, 1, 2)
        self._check_point(L1, L2, L3, -56, 58)

    def test_5_two_near_left(self):
        # L1: x-y=-113, ∩(x+y=2) → (-55.5, 57.5)
        L1 = line1(-113, 1, 0, 1)
        L2 = line2(2, 2)
        L3 = line3(0, 2, 1, 1)
        self._check_point(L1, L2, L3, -55.5, 57.5)

    def test_6_two_near_right(self):
        # L1: x-y=113, ∩(x+y=2) → (57.5, -55.5)
        L1 = line1(113, 1, 0, 1)
        L2 = line2(2, 2)
        L3 = line3(0, 2, 1, 1)
        self._check_point(L1, L2, L3, 57.5, -55.5)


class TestAnalyzeTwoPoints(unittest.TestCase):
    """Клас 4: Дві точки перетину"""

    L1b = line1(0, 1, 0, 1)   # y=x
    L2b = line2(-1, 1)         # x-y+1=0 (пар. до L1)
    L3b = line3(0, 0, 1, 1)   # x+y=0

    def _check(self, L1, L2, L3):
        out = get_result(L1, L2, L3)
        self.assertIn("Дві точки", out)

    def test_1_left_bound(self):
        self._check(line1(-114,1,-114,1), line2(-1,1), line3(-114,0,1,1))

    def test_2_right_bound(self):
        self._check(line1(114,1,114,1), line2(-1,1), line3(114,0,1,1))

    def test_3_middle(self):
        self._check(self.L1b, self.L2b, self.L3b)

    def test_4_left_mid_right(self):
        self._check(line1(-114,1,0,1), self.L2b, line3(0,0,1,1))

    def test_5_two_near_left(self):
        self._check(line1(-113,1,-113,1), line2(-1,1), line3(-113,0,1,1))

    def test_6_two_near_right(self):
        self._check(line1(113,1,113,1), line2(-1,1), line3(113,0,1,1))


class TestAnalyzeThreePoints(unittest.TestCase):
    """Клас 5: Три точки перетину"""

    L1c = line1(0, 1, 0, 1)   # y=x
    L2c = line2(1, -2)         # y=2x-2
    L3c = line3(1, 0, 1, 1)   # x+y=1

    def _check(self, L1, L2, L3):
        out = get_result(L1, L2, L3)
        self.assertIn("Три точки", out)

    def test_1_left_bound(self):
        self._check(line1(-114,1,-114,1), self.L2c, self.L3c)

    def test_2_right_bound(self):
        self._check(line1(114,1,114,1), self.L2c, self.L3c)

    def test_3_middle(self):
        self._check(self.L1c, self.L2c, self.L3c)

    def test_4_left_mid_right(self):
        self._check(line1(-114,1,0,1), self.L2c, line3(114,114,1,1))

    def test_5_two_near_left(self):
        self._check(line1(-113,1,-113,1), self.L2c, self.L3c)

    def test_6_two_near_right(self):
        self._check(line1(113,1,113,1), self.L2c, self.L3c)


# ═══════════════════════════════════════════════════════════
# Тести некоректних вхідних даних (валідація констант)
# ═══════════════════════════════════════════════════════════

class TestBounds(unittest.TestCase):
    """Перевірка констант меж діапазону"""

    def test_lower_bound_value(self):
        self.assertEqual(LOWER_BOUND, -114)

    def test_upper_bound_value(self):
        self.assertEqual(UPPER_BOUND, 114)

    def test_epsilon_value(self):
        self.assertAlmostEqual(EPSILON, 1e-8)

    def test_out_of_range_low(self):
        # -115 < LOWER_BOUND
        self.assertLess(-115, LOWER_BOUND)

    def test_out_of_range_high(self):
        # 115 > UPPER_BOUND
        self.assertGreater(115, UPPER_BOUND)


class TestLineConversions(unittest.TestCase):
    """Перевірка перетворення параметрів у коефіцієнти A,B,C"""

    def test_line1_basic(self):
        A, B, C = line1(0, 1, 0, 1)   # (x-0)/1=(y-0)/1 → y=x
        self.assertAlmostEqual(A, 1)
        self.assertAlmostEqual(B, -1)
        self.assertAlmostEqual(C, 0)

    def test_line1_nonzero_point(self):
        A, B, C = line1(2, 1, 3, 1)   # (x-2)/1=(y-3)/1 → y=x+1
        # A=1,B=-1,C=-1*2+1*3=1
        self.assertAlmostEqual(A, 1)
        self.assertAlmostEqual(B, -1)
        self.assertAlmostEqual(C, 1)

    def test_line2_basic(self):
        A, B, C = line2(2, 3)   # x/2+y/3=1 → 3x+2y=6
        self.assertAlmostEqual(A, 3)
        self.assertAlmostEqual(B, 2)
        self.assertAlmostEqual(C, -6)

    def test_line3_basic(self):
        A, B, C = line3(1, 2, 3, 4)   # 3(x-1)+4(y-2)=0 → 3x+4y=11
        self.assertAlmostEqual(A, 3)
        self.assertAlmostEqual(B, 4)
        self.assertAlmostEqual(C, -11)

    def test_line3_origin(self):
        A, B, C = line3(0, 0, 1, 1)   # x+y=0
        self.assertAlmostEqual(C, 0)


if __name__ == "__main__":
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    for cls in [TestIsZero, TestLinesCoincide, TestLinesParallel,
                TestIntersection, TestPointsEqual, TestFmtPoint,
                TestAnalyzeNoIntersection, TestAnalyzeOnePoint,
                TestAnalyzeTwoPoints, TestAnalyzeThreePoints,
                TestBounds, TestLineConversions]:
        suite.addTests(loader.loadTestsFromTestCase(cls))

    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    sys.exit(0 if result.wasSuccessful() else 1)