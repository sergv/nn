#include <immintrin.h>

#include <arithmetic.h>

#include <stdio.h>

#define PREFETCH_RO_ONESHOT(x) __builtin_prefetch(x, 0, 0)
#define PREFETCH_RW_ONESHOT(x) __builtin_prefetch(x, 1, 0)

void addf(int n,
          const float_aligned * __restrict xs,
          const float_aligned * __restrict ys,
          float_aligned * __restrict zs)
{
        int i = 0;
        for (i = 0; i < n; i++) {
                zs[i] = xs[i] + ys[i];
        }
}

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

void addScaledf(int n,
                const float_aligned * __restrict xs,
                float c,
                const float_aligned * __restrict ys,
                float_aligned * __restrict zs)
{
        int i = 0;
        for (i = 0; i < n; i++) {
                zs[i] = xs[i] + c * ys[i];
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

float dotf(int n,
           const float_aligned * __restrict xs,
           const float_aligned * __restrict ys)
{
        int i = 0;
        const float zero = 0.0f;
        __m256 result = _mm256_broadcast_ss(&zero);
        __m256 x, y;

        const int m  = n / 8;
        const int m8 = m * 8;
        const int k  = n - m8;
        for (i = 0; i < m; i++) {
                x = _mm256_load_ps(&xs[i * 8]);
                y = _mm256_load_ps(&ys[i * 8]);
                /* result = _mm256_add_ps(result, _mm256_mul_ps(x, y)); */
                result = _mm256_fmadd_ps(x, y, result);
        }
        float res[4] __attribute__ ((aligned (FLOAT_ALIGNMENT)));
        _mm256_store_ps(res, result);

        float final_res = res[0] + res[1] + res[2] + res[3];
        for (i = m8; i < n; i++) {
                final_res += xs[i] * ys[i];
        }

        return final_res;
}

double dot(int n,
           const double_aligned * __restrict xs,
           const double_aligned * __restrict ys)
{
        int i = 0;
        const double zero = 0.0;
        __m256d result = _mm256_broadcast_sd(&zero);
        __m256d x, y;

        const int m  = n / 4;
        const int m4 = m * 4;
        const int k  = n - m4;
        for (i = 0; i < m; i++) {
                x = _mm256_load_pd(&xs[i * 4]);
                y = _mm256_load_pd(&ys[i * 4]);
                /* result = _mm256_add_pd(result, _mm256_mul_pd(x, y)); */
                result = _mm256_fmadd_pd(x, y, result);
        }
        double res[4] __attribute__ ((aligned (DOUBLE_ALIGNMENT)));
        _mm256_store_pd(res, result);

        double final_res = res[0] + res[1] + res[2] + res[3];
        for (i = m4; i < n; i++) {
                final_res += xs[i] * ys[i];
        }

        return final_res;

        //int i = 0;
        //double result = 0.0;
        //for (i = 0; i < n; i++) {
        //        result += xs[i] * ys[i];
        //}
        //return result;
}
