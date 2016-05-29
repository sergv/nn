#include <immintrin.h> /* SSE4 + AVX intrinsics */
#include <stdio.h>

#include <arithmetic.h>
#include <arithmetic_types.h>

#include <simd_expf.h>
#include <simd_exp.h>

/* Functions on floats */

#define PREFETCH_RO_ONESHOT(x) __builtin_prefetch(x, 0, 0)
#define PREFETCH_RW_ONESHOT(x) __builtin_prefetch(x, 1, 0)

void addf(unsigned int n,
          const float_aligned * __restrict xs,
          const float_aligned * __restrict ys,
          float_aligned * __restrict zs)
{
        unsigned int i = 0;
        for (i = 0; i < n; i++) {
                zs[i] = xs[i] + ys[i];
        }
}

void add(unsigned int n,
         const double_aligned * __restrict xs,
         const double_aligned * __restrict ys,
         double_aligned * __restrict zs)
{
        unsigned int i = 0;
        for (i = 0; i < n; i++) {
                zs[i] = xs[i] + ys[i];
                // PREFETCH_RO_ONESHOT(&xs[i+1]);
                // PREFETCH_RO_ONESHOT(&ys[i+1]);
                // PREFETCH_RW_ONESHOT(&zs[i+1]);
        }
}

void addScaledf(unsigned int n,
                const float_aligned * __restrict xs,
                float c,
                const float_aligned * __restrict ys,
                float_aligned * __restrict zs)
{
        unsigned int i = 0;
        for (i = 0; i < n; i++) {
                zs[i] = xs[i] + c * ys[i];
        }
}

void addScaled(unsigned int n,
               const double_aligned * __restrict xs,
               double c,
               const double_aligned * __restrict ys,
               double_aligned * __restrict zs)
{
        unsigned int i = 0;
        for (i = 0; i < n; i++) {
                zs[i] = xs[i] + c * ys[i];
        }
}

float dotf(unsigned int n,
           const float_aligned * __restrict xs,
           const float_aligned * __restrict ys)
{
        unsigned int i = 0;
        __m256 result = _mm256_set1_ps(0.0f);
        __m256 x, y;

        const unsigned int m  = n >> 3; // n / 8;
        const unsigned int m8 = m << 3; // m * 8;
        // const unsigned int k  = n - m8;
        for (i = 0; i < m; i++) {
                x = _mm256_load_ps(&xs[i * 8]);
                y = _mm256_load_ps(&ys[i * 8]);
                /* result = _mm256_add_ps(result, _mm256_mul_ps(x, y)); */
                result = _mm256_fmadd_ps(x, y, result);
        }
        float res[8] FLOAT_ALIGNED;
        _mm256_store_ps(res, result);

        float final_res = res[0] + res[1] + res[2] + res[3] + res[4] + res[5] + res[6] + res[7];
        for (i = m8; i < n; i++) {
                final_res += xs[i] * ys[i];
        }

        return final_res;
}

double dot(unsigned int n,
           const double_aligned * __restrict xs,
           const double_aligned * __restrict ys)
{
        unsigned int i = 0;
        __m256d result = _mm256_set1_pd(0.0);
        __m256d x, y;

        const unsigned int m  = n >> 2; // n / 4;
        const unsigned int m4 = m << 2; // m * 4;
        // const unsigned int k  = n - m4;
        for (i = 0; i < m; i++) {
                x = _mm256_load_pd(&xs[i * 4]);
                y = _mm256_load_pd(&ys[i * 4]);
                /* result = _mm256_add_pd(result, _mm256_mul_pd(x, y)); */
                result = _mm256_fmadd_pd(x, y, result);
        }
        double res[4] DOUBLE_ALIGNED;
        _mm256_store_pd(res, result);

        double final_res = res[0] + res[1] + res[2] + res[3];
        for (i = m4; i < n; i++) {
                final_res += xs[i] * ys[i];
        }

        return final_res;

        //unsigned int i = 0;
        //double result = 0.0;
        //for (i = 0; i < n; i++) {
        //        result += xs[i] * ys[i];
        //}
        //return result;
}


__m256 simd_sigmoidf(__m256 x)
{
        const __m256 y = simd_expf(x);
        return _mm256_div_ps(y, _mm256_add_ps(y, _mm256_set1_ps(1.0f)));
}

__m256 simd_sigmoid_derivf(__m256 x)
{
        const __m256 y = simd_expf(x);
        const __m256 y_plus_1 = _mm256_add_ps(y, _mm256_set1_ps(1.0f));
        return _mm256_div_ps(y, _mm256_mul_ps(y_plus_1, y_plus_1));
}

__m256 simd_tanhf(__m256 x)
{
        const __m256 y = simd_expf(x);
        const __m256 z = simd_expf(_mm256_mul_ps(_mm256_set1_ps(-1.0f), x));
        return _mm256_div_ps(_mm256_sub_ps(y, z), _mm256_add_ps(y, z));
}

__m256 simd_tanh_derivf(__m256 x)
{
        const __m256 y = simd_tanhf(x);
        return _mm256_sub_ps(_mm256_set1_ps(1.0f), _mm256_mul_ps(y, y));
}

#define MAP_FUNC_FLOAT(NAME, SIMD_FUNC)                                 \
        void NAME(unsigned int n,                                       \
                  const float_aligned * __restrict xs,                  \
                  float_aligned * __restrict ys)                        \
        {                                                               \
                unsigned int i = 0, j = 0;                              \
                const unsigned int m  = n >> 3;                         \
                const unsigned int m8 = m << 3;                         \
                for (i = 0; i < m; i++) {                               \
                        __m256 x = _mm256_load_ps(&xs[i * 8]);          \
                        _mm256_store_ps(&ys[i * 8], SIMD_FUNC(x));      \
                }                                                       \
                                                                        \
                float leftover[8] FLOAT_ALIGNED = { 0, 0, 0, 0, 0, 0, 0, 0 }; \
                for (i = m8, j = 0; i < n; i++, j++) {                  \
                        leftover[j] = xs[i];                            \
                }                                                       \
                                                                        \
                const __m256 x = SIMD_FUNC(_mm256_load_ps(leftover));   \
                _mm256_store_ps(leftover, x);                           \
                                                                        \
                for (i = m8, j = 0; i < n; i++, j++) {                  \
                        ys[i] = leftover[j];                            \
                }                                                       \
        }

MAP_FUNC_FLOAT(map_expf, simd_expf)
MAP_FUNC_FLOAT(map_sigmoidf, simd_sigmoidf)
MAP_FUNC_FLOAT(map_sigmoid_derivf, simd_sigmoid_derivf)
MAP_FUNC_FLOAT(map_tanhf, simd_tanhf)
MAP_FUNC_FLOAT(map_tanh_derivf, simd_tanh_derivf)

/* Nonlinearity with deriv, float */

__attribute__((always_inline))
inline void simd_sigmoid_with_derivf(__m256 x, __m256 * __restrict nonlin, __m256 * __restrict deriv)
{
        const __m256 y        = simd_expf(x);
        const __m256 y_plus_1 = _mm256_add_ps(y, _mm256_set1_ps(1.0f));
        *nonlin = _mm256_div_ps(y, y_plus_1);
        *deriv  = _mm256_div_ps(*nonlin, y_plus_1);
}

__attribute__((always_inline))
inline void simd_tanh_with_derivf(__m256 x, __m256 * __restrict nonlin, __m256 * __restrict deriv)
{
        const __m256 e_x       = simd_expf(x);
        const __m256 e_minus_x = simd_expf(_mm256_mul_ps(_mm256_set1_ps(-1.0f), x));
        *nonlin = _mm256_div_ps(_mm256_sub_ps(e_x, e_minus_x), _mm256_add_ps(e_x, e_minus_x));
        *deriv  = _mm256_sub_ps(_mm256_set1_ps(1.0f), _mm256_mul_ps(*nonlin, *nonlin));;
}

#define MAP_FUNC_WITH_DERIV_FLOAT(NAME, FUNC)                           \
        void NAME(unsigned int n,                                       \
                  const float_aligned * __restrict xs,                  \
                  float_aligned * __restrict values,                    \
                  float_aligned * __restrict derivs)                    \
        {                                                               \
                unsigned int i = 0, j = 0;                              \
                const unsigned int m  = n >> 3;                         \
                const unsigned int m8 = m << 3;                         \
                                                                        \
                __m256 nonlin, deriv;                                   \
                for (i = 0; i < m; i++) {                               \
                        __m256 x = _mm256_load_ps(&xs[i * 8]);          \
                                                                        \
                        FUNC(x, &nonlin, &deriv);                       \
                                                                        \
                        _mm256_store_ps(&values[i * 8], nonlin);        \
                        _mm256_store_ps(&derivs[i * 8], deriv);         \
                }                                                       \
                                                                        \
                float leftover[8] FLOAT_ALIGNED = { 0, 0, 0, 0, 0, 0, 0, 0 }; \
                for (i = m8, j = 0; i < n; i++, j++) {                  \
                        leftover[j] = xs[i];                            \
                }                                                       \
                                                                        \
                const __m256 x = _mm256_load_ps(leftover);              \
                FUNC(x, &nonlin, &deriv);                               \
                                                                        \
                _mm256_store_ps(leftover, nonlin);                      \
                for (i = m8, j = 0; i < n; i++, j++) {                  \
                        values[i] = leftover[j];                        \
                }                                                       \
                                                                        \
                _mm256_store_ps(leftover, deriv);                       \
                for (i = m8, j = 0; i < n; i++, j++) {                  \
                        derivs[i] = leftover[j];                        \
                }                                                       \
        }

MAP_FUNC_WITH_DERIV_FLOAT(map_sigmoid_with_derivf, simd_sigmoid_with_derivf)
MAP_FUNC_WITH_DERIV_FLOAT(map_tanh_with_derivf, simd_tanh_with_derivf)


/* Functions on doubles */

__m256d simd_sigmoid(__m256d x)
{
        const __m256d y = simd_exp(x);
        return _mm256_div_pd(y, _mm256_add_pd(y, _mm256_set1_pd(1.0)));
}

__m256d simd_sigmoid_deriv(__m256d x)
{
        const __m256d y = simd_exp(x);
        const __m256d y_plus_1 = _mm256_add_pd(y, _mm256_set1_pd(1.0));
        return _mm256_div_pd(y, _mm256_mul_pd(y_plus_1, y_plus_1));
}

__m256d simd_tanh(__m256d x)
{
        const __m256d y = simd_exp(x);
        const __m256d z = simd_exp(_mm256_mul_pd(_mm256_set1_pd(-1.0), x));
        return _mm256_div_pd(_mm256_sub_pd(y, z), _mm256_add_pd(y, z));
}

__m256d simd_tanh_deriv(__m256d x)
{
        const __m256d y = simd_tanh(x);
        return _mm256_sub_pd(_mm256_set1_pd(1.0), _mm256_mul_pd(y, y));
}

#define MAP_FUNC_DOUBLE(NAME, SIMD_FUNC)                                \
        void NAME(unsigned int n,                                       \
                  const double_aligned * __restrict xs,                 \
                  double_aligned * __restrict ys)                       \
        {                                                               \
                unsigned int i = 0, j = 0;                              \
                const unsigned int m  = n >> 2;                         \
                const unsigned int m8 = m << 2;                         \
                for (i = 0; i < m; i++) {                               \
                        __m256d x = _mm256_load_pd(&xs[i * 4]);         \
                        _mm256_store_pd(&ys[i * 4], SIMD_FUNC(x));      \
                }                                                       \
                                                                        \
                double leftover[4] DOUBLE_ALIGNED = { 0, 0, 0, 0 };     \
                for (i = m8, j = 0; i < n; i++, j++) {                  \
                        leftover[j] = xs[i];                            \
                }                                                       \
                                                                        \
                __m256d x = SIMD_FUNC(_mm256_load_pd(leftover));        \
                _mm256_store_pd(leftover, x);                           \
                                                                        \
                for (i = m8, j = 0; i < n; i++, j++) {                  \
                        ys[i] = leftover[j];                            \
                }                                                       \
        }

MAP_FUNC_DOUBLE(map_exp, simd_exp)
MAP_FUNC_DOUBLE(map_sigmoid, simd_sigmoid)
MAP_FUNC_DOUBLE(map_sigmoid_deriv, simd_sigmoid_deriv)
MAP_FUNC_DOUBLE(map_tanh, simd_tanh)
MAP_FUNC_DOUBLE(map_tanh_deriv, simd_tanh_deriv)

/* Nonlinearity with deriv, double */

__attribute__((always_inline))
inline void simd_sigmoid_with_deriv(__m256d x, double_aligned * __restrict nonlin, double_aligned * __restrict deriv)
{
        const __m256d e_x        = simd_exp(x);
        const __m256d e_x_plus_1 = _mm256_add_pd(e_x, _mm256_set1_pd(1.0));
        const __m256d y          = _mm256_div_pd(e_x, e_x_plus_1);
        const __m256d dy         = _mm256_div_pd(dy, e_x_plus_1);
        _mm256_store_pd(nonlin, y);
        _mm256_store_pd(deriv, dy);
}

__attribute__((always_inline))
inline void simd_tanh_with_deriv(__m256d x, double_aligned * __restrict nonlin, double_aligned * __restrict deriv)
{
        const __m256d e_x       = simd_exp(x);
        const __m256d e_minus_x = simd_exp(_mm256_mul_pd(_mm256_set1_pd(-1.0), x));
        const __m256d y         = _mm256_div_pd(_mm256_sub_pd(e_x, e_minus_x), _mm256_add_pd(e_x, e_minus_x));
        const __m256d dy        = _mm256_sub_pd(_mm256_set1_pd(1.0), _mm256_mul_pd(y, y));
        _mm256_store_pd(nonlin, y);
        _mm256_store_pd(deriv, dy);
}

#define MAP_FUNC_WITH_DERIV(NAME, FUNC)                                 \
        void NAME(unsigned int n,                                       \
                  const double_aligned * __restrict xs,                 \
                  double_aligned * __restrict values,                   \
                  double_aligned * __restrict derivs)                   \
        {                                                               \
                unsigned int i = 0, j = 0;                              \
                const unsigned int m  = n >> 2;                         \
                const unsigned int m4 = m << 2;                         \
                                                                        \
                for (i = 0; i < m; i++) {                               \
                        const __m256d x = _mm256_load_pd(&xs[i * 4]);   \
                                                                        \
                        FUNC(x, &values[i * 4], &derivs[i * 4]);        \
                }                                                       \
                                                                        \
                double leftover[4] DOUBLE_ALIGNED = { 0, 0, 0, 0 };     \
                for (i = m4, j = 0; i < n; i++, j++) {                  \
                        leftover[j] = xs[i];                            \
                }                                                       \
                                                                        \
                const __m256d x = _mm256_load_pd(leftover);             \
                double nonlin_store[4] DOUBLE_ALIGNED;                  \
                double deriv_store[4] DOUBLE_ALIGNED;                   \
                FUNC(x, nonlin_store, deriv_store);                     \
                                                                        \
                for (i = m4, j = 0; i < n; i++, j++) {                  \
                        values[i] = nonlin_store[j];                    \
                        derivs[i] = deriv_store[j];                     \
                }                                                       \
        }

MAP_FUNC_WITH_DERIV(map_sigmoid_with_deriv, simd_sigmoid_with_deriv)
MAP_FUNC_WITH_DERIV(map_tanh_with_deriv, simd_tanh_with_deriv)
