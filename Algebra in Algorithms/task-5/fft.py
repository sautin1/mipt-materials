import math


class FFT:
    @staticmethod
    def transform(coeffs):
        if len(coeffs) == 1:
            return coeffs.copy()
        even_coeffs, odd_coeffs = [], []
        for i, coeff in enumerate(coeffs):
            [even_coeffs, odd_coeffs][i % 2].append(coeff)
        even_coeffs, odd_coeffs = FFT.transform(even_coeffs), FFT.transform(odd_coeffs)
        result = [0] * len(coeffs)
        angle = 2 * math.pi / len(coeffs)
        omega, omega_factor = 1, math.cos(angle) + 1j * math.sin(angle)
        for i, (even_coef, odd_coef) in enumerate(zip(even_coeffs, odd_coeffs)):
            result[i] = even_coef + omega * odd_coef
            result[len(coeffs) // 2 + i] = even_coef - omega * odd_coef
            omega *= omega_factor
        return result


if __name__ == '__main__':
    coeffs = list(map(int, input().split()))
    values = FFT.transform(coeffs)
    print(' '.join(f'{value.real},{value.imag}' for value in values))
