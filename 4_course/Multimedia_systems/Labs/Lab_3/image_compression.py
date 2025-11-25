import sys

import numpy as np
import cv2
import matplotlib.pyplot as plt
import os
import warnings

warnings.filterwarnings('ignore', category=RuntimeWarning)

# Створюємо папку для результатів
OUTPUT_DIR = 'outputs'
os.makedirs(OUTPUT_DIR, exist_ok=True)


class ImageCompression:
    def __init__(self, image_path: str):
        self.original = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)
        if self.original is None:
            raise ValueError(f"Не вдалося завантажити зображення: {image_path}")

        # Приведення до розміру кратного 8
        h, w = self.original.shape
        new_h = (h // 8) * 8
        new_w = (w // 8) * 8
        self.original = self.original[:new_h, :new_w]

        print(f"Розмір зображення: {self.original.shape}")

    # ==================== DCT====================

    def dct_matrix(self, N: int) -> np.ndarray:
        """
        Створення базисної матриці DCT розміром NxN

        DCT матриця T[i,j] = α(i) * cos(π * i * (2j + 1) / (2N))
        де α(0) = sqrt(1/N), α(i) = sqrt(2/N) для i > 0
        """
        T = np.zeros((N, N))

        for i in range(N):
            for j in range(N):
                if i == 0:
                    alpha = np.sqrt(1.0 / N)
                else:
                    alpha = np.sqrt(2.0 / N)

                T[i, j] = alpha * np.cos(np.pi * i * (2 * j + 1) / (2.0 * N))

        return T

    def dct2d(self, block: np.ndarray) -> np.ndarray:
        """
        2D DCT перетворення для блоку за допомогою матричного множення

        F = T * f * T^T
        де T - базисна матриця DCT, f - вхідний блок
        """
        N = block.shape[0]
        T = self.dct_matrix(N)

        # F = T * f * T^T
        dct_coeffs = T @ block @ T.T

        return dct_coeffs

    def idct2d(self, dct_coeffs: np.ndarray) -> np.ndarray:
        """
        Зворотнє 2D DCT перетворення

        f = T^T * F * T
        """
        N = dct_coeffs.shape[0]
        T = self.dct_matrix(N)

        # f = T^T * F * T
        reconstructed = T.T @ dct_coeffs @ T

        return reconstructed

    def dct_compress(self, quality_factor: float = 0.5):
        """
        Стиснення зображення методом DCT (власна реалізація)
        """
        h, w = self.original.shape
        compressed = np.zeros_like(self.original, dtype=float)

        block_size = 8
        kept_coeffs = 0
        total_coeffs = 0

        print(f"\n[DCT] Обробка {(h // block_size) * (w // block_size)} блоків 8×8...")

        # Обробка по блоках 8x8
        for i in range(0, h, block_size):
            for j in range(0, w, block_size):
                block = self.original[i:i + block_size, j:j + block_size].astype(float)

                # DCT
                dct_block = self.dct2d(block)

                # Квантування (обнулення малих коефіцієнтів)
                threshold = np.percentile(np.abs(dct_block), (1 - quality_factor) * 100)
                mask = np.abs(dct_block) >= threshold
                dct_block[~mask] = 0

                kept_coeffs += np.sum(mask)
                total_coeffs += block_size * block_size

                # Зворотній DCT
                reconstructed = self.idct2d(dct_block)
                compressed[i:i + block_size, j:j + block_size] = reconstructed

        compression_ratio = total_coeffs / kept_coeffs if kept_coeffs > 0 else 1.0
        print(f"[DCT] Збережено {kept_coeffs}/{total_coeffs} коефіцієнтів ({kept_coeffs * 100 / total_coeffs:.1f}%)")

        return np.clip(compressed, 0, 255).astype(np.uint8), compression_ratio

    # ==================== WAVELET====================

    def haar_transform_1d(self, signal: np.ndarray) -> tuple:
        """
        Одновимірне вейвлет-перетворення Хаара

        Апроксимація: a[i] = (x[2i] + x[2i+1]) / sqrt(2)
        Деталізація:  d[i] = (x[2i] - x[2i+1]) / sqrt(2)
        """
        n = len(signal)
        if n % 2 != 0:
            signal = np.append(signal, signal[-1])
            n += 1

        approx = np.zeros(n // 2)
        detail = np.zeros(n // 2)

        sqrt2 = np.sqrt(2.0)

        for i in range(n // 2):
            approx[i] = (signal[2 * i] + signal[2 * i + 1]) / sqrt2
            detail[i] = (signal[2 * i] - signal[2 * i + 1]) / sqrt2

        return approx, detail

    def haar_inverse_1d(self, approx: np.ndarray, detail: np.ndarray) -> np.ndarray:
        """
        Зворотнє одновимірне вейвлет-перетворення Хаара

        x[2i]   = (a[i] + d[i]) / sqrt(2)
        x[2i+1] = (a[i] - d[i]) / sqrt(2)
        """
        n = len(approx)
        signal = np.zeros(2 * n)

        sqrt2 = np.sqrt(2.0)

        for i in range(n):
            signal[2 * i] = (approx[i] + detail[i]) / sqrt2
            signal[2 * i + 1] = (approx[i] - detail[i]) / sqrt2

        return signal

    def haar_transform_2d(self, image: np.ndarray) -> tuple:
        """
        Двовимірне вейвлет-перетворення Хаара

        Спочатку застосовуємо по рядках, потім по стовпцях
        Результат: LL (апроксимація), LH, HL, HH (деталі)
        """
        h, w = image.shape

        # Крок 1: Перетворення по рядках
        row_transformed = np.zeros((h, w))
        for i in range(h):
            approx, detail = self.haar_transform_1d(image[i, :])
            row_transformed[i, :w // 2] = approx
            row_transformed[i, w // 2:] = detail

        # Крок 2: Перетворення по стовпцях
        result = np.zeros((h, w))

        # Апроксимація (ліва половина)
        for j in range(w // 2):
            approx, detail = self.haar_transform_1d(row_transformed[:, j])
            result[:h // 2, j] = approx
            result[h // 2:, j] = detail

        # Деталізація (права половина)
        for j in range(w // 2, w):
            approx, detail = self.haar_transform_1d(row_transformed[:, j])
            result[:h // 2, j] = approx
            result[h // 2:, j] = detail

        # Розділення на компоненти
        LL = result[:h // 2, :w // 2]
        LH = result[:h // 2, w // 2:]
        HL = result[h // 2:, :w // 2]
        HH = result[h // 2:, w // 2:]

        return LL, (LH, HL, HH)

    def haar_inverse_2d(self, LL: np.ndarray, details: tuple) -> np.ndarray:
        """
        Зворотнє двовимірне вейвлет-перетворення Хаара
        """
        LH, HL, HH = details
        h, w = LL.shape[0] * 2, LL.shape[1] * 2

        # Збираємо компоненти назад
        result = np.zeros((h, w))
        result[:h // 2, :w // 2] = LL
        result[:h // 2, w // 2:] = LH
        result[h // 2:, :w // 2] = HL
        result[h // 2:, w // 2:] = HH

        # Зворотнє перетворення по стовпцях
        col_transformed = np.zeros((h, w))

        for j in range(w // 2):
            col_transformed[:, j] = self.haar_inverse_1d(result[:h // 2, j], result[h // 2:, j])

        for j in range(w // 2, w):
            col_transformed[:, j] = self.haar_inverse_1d(result[:h // 2, j], result[h // 2:, j])

        # Зворотнє перетворення по рядках
        reconstructed = np.zeros((h, w))
        for i in range(h):
            reconstructed[i, :] = self.haar_inverse_1d(
                col_transformed[i, :w // 2],
                col_transformed[i, w // 2:]
            )

        return reconstructed

    def wavelet_compress_multilevel(self, image: np.ndarray, level: int, quality_factor: float):
        """
        Багаторівневе вейвлет стиснення
        """
        coeffs = []
        current = image.astype(float)

        for lev in range(level):
            LL, details = self.haar_transform_2d(current)

            # Порогова обробка деталізуючих коефіцієнтів
            LH, HL, HH = details
            all_details = np.concatenate([LH.flatten(), HL.flatten(), HH.flatten()])
            threshold = np.percentile(np.abs(all_details), (1 - quality_factor) * 100)

            LH_thresh = np.where(np.abs(LH) >= threshold, LH, 0)
            HL_thresh = np.where(np.abs(HL) >= threshold, HL, 0)
            HH_thresh = np.where(np.abs(HH) >= threshold, HH, 0)

            coeffs.append((LH_thresh, HL_thresh, HH_thresh))
            current = LL

        coeffs.append(LL)  # Остання апроксимація

        return coeffs

    def wavelet_reconstruct_multilevel(self, coeffs: list) -> np.ndarray:
        """
        Багаторівнева реконструкція
        """
        LL = coeffs[-1]

        for i in range(len(coeffs) - 2, -1, -1):
            details = coeffs[i]
            LL = self.haar_inverse_2d(LL, details)

        return LL

    def wavelet_compress(self, level: int = 3, quality_factor: float = 0.5):
        """
        Стиснення зображення методом вейвлет-перетворення Хаара
        """
        print(f"\n[Wavelet] Багаторівнева декомпозиція ({level} рівнів)...")

        # Багаторівнева декомпозиція
        coeffs = self.wavelet_compress_multilevel(self.original, level, quality_factor)

        # Підрахунок коефіцієнта стиснення
        kept_coeffs = coeffs[-1].size  # LL компонента
        total_coeffs = self.original.size

        for i in range(len(coeffs) - 1):
            LH, HL, HH = coeffs[i]
            kept_coeffs += np.sum(np.abs(LH) > 0)
            kept_coeffs += np.sum(np.abs(HL) > 0)
            kept_coeffs += np.sum(np.abs(HH) > 0)

        print(
            f"[Wavelet] Збережено {kept_coeffs}/{total_coeffs} коефіцієнтів ({kept_coeffs * 100 / total_coeffs:.1f}%)")

        # Реконструкція
        reconstructed = self.wavelet_reconstruct_multilevel(coeffs)

        # Обрізання до оригінального розміру
        h, w = self.original.shape
        reconstructed = reconstructed[:h, :w]

        # Безпечна обробка
        reconstructed = np.nan_to_num(reconstructed, nan=0.0, posinf=255.0, neginf=0.0)

        compression_ratio = total_coeffs / kept_coeffs if kept_coeffs > 0 else 1.0

        return np.clip(reconstructed, 0, 255).astype(np.uint8), compression_ratio

    # ==================== МЕТРИКИ ЯКОСТІ ====================

    @staticmethod
    def calculate_psnr(original: np.ndarray, compressed: np.ndarray) -> float:
        """
        Обчислення PSNR (Peak Signal-to-Noise Ratio)

        PSNR = 20 * log10(MAX_I / sqrt(MSE))
        де MSE = mean((original - compressed)^2)
        """
        mse = np.mean((original.astype(float) - compressed.astype(float)) ** 2)

        if mse == 0:
            return float('inf')

        max_pixel = 255.0
        psnr = 20 * np.log10(max_pixel / np.sqrt(mse))

        return psnr

    # ==================== ПОРІВНЯННЯ ====================

    def compare_methods(self, quality_factors: list = [0.3, 0.5, 0.7]):
        """
        Порівняння методів стиснення
        """
        results = {
            'DCT': [],
            'Wavelet Haar': []
        }

        print("\n" + "=" * 80)
        print("ПОРІВНЯННЯ МЕТОДІВ СТИСНЕННЯ")
        print("=" * 80)
        print(f"\n{'Метод':<25} {'Якість':<10} {'PSNR (дБ)':<12} {'Стиснення':<12}")
        print("-" * 80)

        for qf in quality_factors:
            # DCT стиснення
            dct_compressed, dct_ratio = self.dct_compress(quality_factor=qf)
            dct_psnr = self.calculate_psnr(self.original, dct_compressed)
            results['DCT'].append((qf, dct_psnr, dct_compressed, dct_ratio))
            print(f"{'DCT':<25} {qf:<10.1f} {dct_psnr:<12.2f} {dct_ratio:<12.2f}x")

            # Wavelet стиснення
            wavelet_compressed, wavelet_ratio = self.wavelet_compress(level=3, quality_factor=qf)
            wavelet_psnr = self.calculate_psnr(self.original, wavelet_compressed)
            results['Wavelet Haar'].append((qf, wavelet_psnr, wavelet_compressed, wavelet_ratio))
            print(f"{'Wavelet Haar':<25} {qf:<10.1f} {wavelet_psnr:<12.2f} {wavelet_ratio:<12.2f}x")
            print()

        # Візуалізація
        self._plot_results(results, quality_factors)

        return results

    def _plot_results(self, results: dict, quality_factors: list):
        """Візуалізація результатів"""

        # Графік 1: Порівняння для середнього QF
        mid_idx = len(quality_factors) // 2
        mid_qf = quality_factors[mid_idx]

        fig, axes = plt.subplots(2, 3, figsize=(14, 8))
        fig.suptitle(f'Порівняння методів (QF = {mid_qf})',
                     fontsize=14, fontweight='bold')

        # Оригінал
        axes[0, 0].imshow(self.original, cmap='gray', vmin=0, vmax=255)
        axes[0, 0].set_title('Оригінал', fontsize=11, fontweight='bold')
        axes[0, 0].axis('off')

        axes[1, 0].text(0.5, 0.5, 'Карти помилок', ha='center', va='center',
                        fontsize=11, fontweight='bold')
        axes[1, 0].axis('off')

        # Методи
        for idx, (method_name, method_results) in enumerate(results.items(), 1):
            qf, psnr, compressed, ratio = method_results[mid_idx]

            # Зображення
            axes[0, idx].imshow(compressed, cmap='gray', vmin=0, vmax=255)
            axes[0, idx].set_title(f'{method_name}\nPSNR={psnr:.2f}дБ, {ratio:.2f}x',
                                   fontsize=10)
            axes[0, idx].axis('off')

            # Помилка
            error = np.abs(self.original.astype(float) - compressed.astype(float))
            im = axes[1, idx].imshow(error, cmap='hot', vmin=0, vmax=50)
            axes[1, idx].set_title(f'Макс: {error.max():.1f}', fontsize=10)
            axes[1, idx].axis('off')

        plt.colorbar(im, ax=axes[1, 2], fraction=0.046)
        plt.tight_layout()
        plt.savefig(os.path.join(OUTPUT_DIR, 'compression_comparison.png'),
                    dpi=150, bbox_inches='tight')
        print("\n✓ Графік збережено: compression_comparison.png")

        # Графік 2: PSNR
        fig2, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 5))

        for method_name, method_results in results.items():
            qfs = [r[0] for r in method_results]
            psnrs = [r[1] for r in method_results]
            ratios = [r[3] for r in method_results]

            ax1.plot(qfs, psnrs, 'o-', linewidth=2, markersize=8, label=method_name)
            ax2.plot(qfs, ratios, 's-', linewidth=2, markersize=8, label=method_name)

        ax1.set_xlabel('Коефіцієнт якості', fontsize=11)
        ax1.set_ylabel('PSNR (дБ)', fontsize=11)
        ax1.set_title('Якість стиснення', fontsize=12, fontweight='bold')
        ax1.legend()
        ax1.grid(True, alpha=0.3)

        ax2.set_xlabel('Коефіцієнт якості', fontsize=11)
        ax2.set_ylabel('Коефіцієнт стиснення', fontsize=11)
        ax2.set_title('Ефективність стиснення', fontsize=12, fontweight='bold')
        ax2.legend()
        ax2.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig(os.path.join(OUTPUT_DIR, 'psnr_analysis.png'),
                    dpi=150, bbox_inches='tight')
        print("✓ Графік PSNR збережено: psnr_analysis.png")


def main():
    print("=" * 80)
    print("РЕАЛІЗАЦІЯ АЛГОРИТМІВ СТИСНЕННЯ ЗОБРАЖЕНЬ")
    print("DCT та Wavelet перетворення")
    print("=" * 80)

    # Отримання шляху до зображення
    if len(sys.argv) > 1:
        # Якщо передано аргумент командного рядка
        image_path = sys.argv[1]
    else:
        # Шлях за замовчуванням
        image_path = 'image.png'

    # Перевірка існування файлу
    if not os.path.exists(image_path):
        print(f"\nФайл не знайдено: {image_path}")
        return

    try:
        # Створення компресора з вашим зображенням
        print(f"\nЗавантаження зображення: {image_path}")
        compressor = ImageCompression(image_path)

        # Збереження оригіналу для порівняння
        cv2.imwrite(os.path.join(OUTPUT_DIR, 'original_image.png'), compressor.original)
        print(f"✓ Оригінал збережено: original_image.png (градації сірого)")

        # Порівняння методів
        results = compressor.compare_methods(quality_factors=[0.3, 0.5, 0.7])

        print("\n" + "=" * 80)
        print(f"\n✓ Всі результати збережено в папці '{OUTPUT_DIR}/'")
        print("\nСтворені файли:")
        print(f"   • original_image.png - зображення в градаціях сірого")
        print(f"   • compression_comparison.png - порівняння методів + карти помилок")
        print(f"   • psnr_analysis.png - графіки PSNR та коефіцієнтів стиснення")

    except Exception as e:
        print(f"\nПомилка обробки: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()