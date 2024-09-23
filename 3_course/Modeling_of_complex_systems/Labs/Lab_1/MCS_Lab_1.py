import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import find_peaks
from scipy.optimize import curve_fit

def load_data(file_path):
    return np.loadtxt(file_path)

def custom_dft(signal):
    N = len(signal)
    dft = np.zeros(N, dtype=complex)
    for k in range(N):
        for n in range(N):
            dft[k] += signal[n] * np.exp(-2j * np.pi * n * k / N)
    return dft

def find_significant_frequencies(dft, dt):
    f_magnitude = np.abs(dft)
    frequencies = np.fft.fftfreq(len(dft), dt)
    peaks, _ = find_peaks(f_magnitude[:len(dft) // 2])
    peak_frequencies = frequencies[peaks]
    return peak_frequencies

def fit_model(t, observations, peak_frequencies):
    def model(t, a1, a2, a3, *params):
        k = len(params) // 2
        y = a1 * t**3 + a2 * t**2 + a3 * t
        for i in range(k):
            fi = params[i]
            ai = params[k + i]
            y += ai * np.sin(2 * np.pi * fi * t - 3 * t)
        return y

    initial_guess = [1, 1, 1] + [1] * (len(peak_frequencies) * 2)
    params, covariance = curve_fit(model, t, observations, p0=initial_guess)
    return params, model(t, *params)

def main():
    observations = load_data('f4.txt')
    dt = 0.01
    t = np.arange(0, len(observations) * dt, dt)

    # 1. Discrete Fourier Transform
    dft = custom_dft(observations)
    peak_frequencies = find_significant_frequencies(dft, dt)
    print("Significant frequencies:", peak_frequencies)

    # 2. Plot the DFT magnitude
    f_magnitude = np.abs(dft)
    frequencies = np.fft.fftfreq(len(dft), dt)
    plt.figure()
    plt.plot(frequencies[:len(dft) // 2], f_magnitude[:len(dft) // 2])
    plt.scatter(peak_frequencies, f_magnitude[find_peaks(f_magnitude[:len(dft) // 2])[0]], color='red')
    plt.xlabel('Frequency')
    plt.ylabel('Magnitude')
    plt.title('DFT Magnitude')
    plt.show()

    # 3. Least-squares fitting
    params, fitted_values = fit_model(t, observations, peak_frequencies)
    print("Found parameters:", params)

    # 4. Plot the original and fitted values
    plt.figure()
    plt.plot(t, observations, label='Observations')
    plt.plot(t, fitted_values, label='Model', linestyle='--')
    plt.xlabel('Time')
    plt.ylabel('y(t)')
    plt.legend()
    plt.title('Comparison of Observations and Model')
    plt.show()

if __name__ == "__main__":
    main()