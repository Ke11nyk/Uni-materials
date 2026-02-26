"""
Лабораторна робота №1 — Варіант 14
Визначення взаємного розміщення трьох прямих на площині.
"""

# ── Реекспорт для зворотної сумісності з тестами ──────────
from lines.constants import LOWER_BOUND, UPPER_BOUND, EPSILON
from lines.geometry import (
    is_zero, lines_coincide, lines_parallel,
    intersection, points_equal, fmt_point,
)
from lines.analyze import analyze_three_lines
from lines.input_lines import (
    read_float, read_int_in_range,
    input_line1, input_line2, input_line3,
)

__all__ = [
    "LOWER_BOUND", "UPPER_BOUND", "EPSILON",
    "is_zero", "lines_coincide", "lines_parallel",
    "intersection", "points_equal", "fmt_point",
    "analyze_three_lines",
    "read_float", "read_int_in_range",
    "input_line1", "input_line2", "input_line3",
]


def main():
    print("=" * 60)
    print("Лабораторна робота №1 — Варіант 14")
    print("Взаємне розміщення трьох прямих на площині")
    print(f"Допустимий діапазон параметрів: [{LOWER_BOUND}; {UPPER_BOUND}]")
    print("=" * 60)

    while True:
        try:
            l1 = input_line1()
            l2 = input_line2()
            l3 = input_line3()

            print("\n--- Загальні рівняння прямих (Ax + By + C = 0) ---")
            for i, (A, B, C) in enumerate([l1, l2, l3], 1):
                print(f"  Пряма {i}: {A:g}x + {B:g}y + {C:g} = 0")

            analyze_three_lines(l1, l2, l3)

        except KeyboardInterrupt:
            print("\n\nРоботу програми завершено.")
            break
        except Exception as e:
            print(f"\n[Критична помилка] {e}")

        again = input("\nПовторити? (т/н): ").strip().lower()
        if again not in ("т", "y", "yes", "так", "t"):
            break


if __name__ == "__main__":
    main()