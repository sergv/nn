#include <immintrin.h>

#include <arithmetic.h>

#include <stdio.h>

#define PREFETCH_RO_ONESHOT(x) __builtin_prefetch(x, 0, 0)
#define PREFETCH_RW_ONESHOT(x) __builtin_prefetch(x, 1, 0)

void add(int n,
         const double_aligned * __restrict xs,
         const double_aligned * __restrict ys,
         double_aligned * __restrict zs)
{
        int i = 0;
        for (i = 0; i < n; i++) {
                zs[i] = xs[i] + ys[i];
                // PREFETCH_RO_ONESHOT(&xs[i+1]);
                // PREFETCH_RO_ONESHOT(&ys[i+1]);
                // PREFETCH_RW_ONESHOT(&zs[i+1]);
        }
}

void addScaled(int n,
               const double_aligned * __restrict xs,
               double c,
               const double_aligned * __restrict ys,
               double_aligned * __restrict zs)
{
        int i = 0;
        for (i = 0; i < n; i++) {
                zs[i] = xs[i] + c * ys[i];
        }
}

double dot(int n,
           const double_aligned * __restrict xs,
           const double_aligned * __restrict ys)
{
        int i = 0;
        const double zero = 0.0;
        __m256d result = _mm256_broadcast_sd(&zero);
        __m256d x, y;

        const int m = n / 4;
        const int k = n - m * 4;
        for (i = 0; i < m; i++) {
                x = _mm256_load_pd(&xs[i * 4]);
                y = _mm256_load_pd(&ys[i * 4]);
                /* result = _mm256_add_pd(result, _mm256_mul_pd(x, y)); */
                result = _mm256_fmadd_pd(x, y, result);
        }
        double res[4];
        _mm256_store_pd(res, result);

        //__m128d xx, yy, zz = _mm128_load_ss(&zero);
        //for (i = m * 4; i < n; i++) {
        //        xx = _mm128_load_sd(&xs[i]);
        //        yy = _mm128_load_sd(&ys[i]);
        //        result = _mm128_fmadd_sd(x, y, result);
        //}
        //double rest = 0.0;
        //_mm128_store_sd(res, result);

        double rest = 0.0;
        for (i = m * 4; i < n; i++) {
                rest += xs[i] * ys[i];
        }

        return rest + res[0] + res[1] + res[2] + res[3];

        //int i = 0;
        //double results[4] = { 0.0, 0.0, 0.0, 0.0 };
        //const int m  = n / 4;
        //const int m4 = m * 4;
        //const int k  = n - m4;
        //for (i = 0; i < m; i++) {
        //        const int j = i * 4;
        //        results[0] += xs[j]     * ys[j];
        //        results[1] += xs[j + 1] * ys[j + 1];
        //        results[2] += xs[j + 2] * ys[j + 2];
        //        results[3] += xs[j + 3] * ys[j + 3];
        //}
        //for (i = m4; i < n; i++) {
        //        results[0] += xs[i] * ys[i];
        //}
        //return results[0] + results[1] + results[2] + results[3];

        //int i = 0;
        //double result = 0.0;
        //for (i = 0; i < n; i++) {
        //        result += xs[i] * ys[i];
        //}
        //return result;
}
