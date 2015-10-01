#include <immintrin.h> /* SSE4 + AVX intrinsics */

#include <arithmetic.h>

#include <stdio.h>

#define PREFETCH_RO_ONESHOT(x) __builtin_prefetch(x, 0, 0)
#define PREFETCH_RW_ONESHOT(x) __builtin_prefetch(x, 1, 0)

#define FLOAT_ALIGNED __attribute__ ((aligned (FLOAT_ALIGNMENT)))
#define DOUBLE_ALIGNED __attribute__ ((aligned (DOUBLE_ALIGNMENT)))

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

#define EXP_HI_F 88.3762626647949f
#define EXP_LO_F -88.3762626647949f

#define EXP_C1_F 0.693359375f
#define EXP_C2_F -2.12194440e-4f
#define LOG2EF_F 1.44269504088896341f

// 1.00000000264427218948278203006432871025920839881386 +
// x * (0.99999963067649002690044369582528379028832484131898 +
//      x * (0.50000841545023142787903870186184144408505393182829 +
//           x * (0.16659493724050791203783110738296245860094527760688 +
//                x * (4.1956255271784880742079192355366894867234413983949e-2 +
//                     x * (7.7400591813870621456134273241091136329932120160017e-3 +
//                          x * 1.9736843567936282408923094691147254439170351115627e-3)))))

// 1 + x + x * x * Y = 1 + x * (1 + x * Y)

/* 1 + x * (1 +
         x * (0.4999999105930328369140625 +
              x * (0.166668415069580078125 +
                   x * (4.16539050638675689697265625e-2 +
                        x * (8.378830738365650177001953125e-3 +
                             x * (1.304379315115511417388916015625e-3 +
                                   x * 2.7555381529964506626129150390625e-4))))))
 */

#define EXP_P0_F 1.0f
#define EXP_P1_F 1.0f
#define EXP_P2_F 0.4999999105930328369140625f
#define EXP_P3_F 0.166668415069580078125f
#define EXP_P4_F 4.16539050638675689697265625e-2f
#define EXP_P5_F 8.378830738365650177001953125e-3f
#define EXP_P6_F 1.304379315115511417388916015625e-3f
#define EXP_P7_F 2.7555381529964506626129150390625e-4f

/* x * rest + c */
#define FMA_F(c, x, rest) FMA_F_LOADED(_mm256_set1_ps(c), x, rest)
#define FMA_F_LOADED(c, x, rest) _mm256_fmadd_ps(x, rest, c)
#define FMA_F_END(c) _mm256_set1_ps(c)

__m256 simd_expf(__m256 x)
{
        const __m256 one = _mm256_set1_ps(1.0f);
        // x = _mm256_min_ps(x, _mm256_set1_ps(EXP_HI_F));
        // x = _mm256_max_ps(x, _mm256_set1_ps(EXP_LO_F));
        const __m256 x_clamped = _mm256_max_ps(_mm256_min_ps(x,
                                                             _mm256_set1_ps(EXP_HI_F)),
                                               _mm256_set1_ps(EXP_LO_F));

        const __m256 fx = _mm256_fmadd_ps(x_clamped,
                                          _mm256_set1_ps(LOG2EF_F),
                                          _mm256_set1_ps(0.5f));

        const __m256 tmp  = _mm256_cvtepi32_ps(_mm256_cvttps_epi32(fx));
        /* mask with ones for elements of tmp where tmp[i] > fx[i] */
        const __m256 mask = _mm256_and_ps(_mm256_cmp_ps(tmp, fx, _CMP_GT_OQ), one);
        const __m256 fx_rounded = _mm256_sub_ps(tmp, mask);

        const __m256 x_reduced = _mm256_sub_ps(
                                   _mm256_sub_ps(x,
                                                 _mm256_mul_ps(fx_rounded,
                                                               _mm256_set1_ps(EXP_C1_F))),
                                   _mm256_mul_ps(fx_rounded,
                                                 _mm256_set1_ps(EXP_C2_F)));
        // _mm256_fmsub_ps(fx_rounded,
        //                 _mm256_set1_ps(EXP_C1_F),
        //                 z);

        const __m256 y =
          FMA_F_LOADED(one,
                       x_reduced,
                       FMA_F_LOADED(one,
                                    x_reduced,
                                    FMA_F(EXP_P2_F,
                                          x_reduced,
                                          FMA_F(EXP_P3_F,
                                                x_reduced,
                                                FMA_F(EXP_P4_F,
                                                      x_reduced,
                                                      FMA_F(EXP_P5_F,
                                                            x_reduced,
                                                            FMA_F(EXP_P6_F,
                                                                  x_reduced,
                                                                  FMA_F_END(EXP_P7_F))))))));

        __m256 pow2n =
          _mm256_castsi256_ps(
            _mm256_slli_epi32(
              _mm256_add_epi32(
                _mm256_cvttps_epi32(fx_rounded),
                _mm256_set1_epi32(0x7f)), // 127
              23));

        return _mm256_mul_ps(y, pow2n);
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


#define EXP_HI_D 7.08396418532264106224E2     /* log (2**1022) */
#define EXP_LO_D -7.08396418532264106224E2    /* log (2**-1022) */

#define EXP_C1_D = 6.93145751953125E-1;
#define EXP_C2_D = 1.42860682030941723212E-6;
#define LOG2EF_D 1.44269504088896341

#define EXP_P0_D 1.0
#define EXP_P1_D 1.00000000000000088817841970012523233890533447265625
#define EXP_P2_D 0.49999999999992245092172993281565140932798385620117
#define EXP_P3_D 0.16666666666900348259261477323889266699552536010742
#define EXP_P4_D 4.1666666631760281014340563388032023794949054718017e-2
#define EXP_P5_D 8.3333336346954715867507701432259636931121349334716e-3
#define EXP_P6_D 1.38888726085892658113074560333188856020569801330566e-3
#define EXP_P7_D 1.98418431473187077554790058719902390294009819626808e-4
#define EXP_P8_D 2.47882381388634570921428862400048842573596630245447e-5
#define EXP_P9_D 2.77605943495647722993780077971326392116679926402867e-6
#define EXP_P10_D 2.56230990287284038811301467228331851799794094404206e-7
#define EXP_P11_D 3.5294625005568410117034257485191250935940843191929e-8

// 1 +
// x * (1.00000000000000088817841970012523233890533447265625 +
//      x * (0.49999999999992245092172993281565140932798385620117 +
//           x * (0.16666666666900348259261477323889266699552536010742 +
//                x * (4.1666666631760281014340563388032023794949054718017e-2 +
//                     x * (8.3333336346954715867507701432259636931121349334716e-3 +
//                          x * (1.38888726085892658113074560333188856020569801330566e-3 +
//                               x * (1.98418431473187077554790058719902390294009819626808e-4 +
//                                    x * (2.47882381388634570921428862400048842573596630245447e-5 +
//                                         x * (2.77605943495647722993780077971326392116679926402867e-6 +
//                                              x * (2.56230990287284038811301467228331851799794094404206e-7 +
//                                                   x * 3.5294625005568410117034257485191250935940843191929e-8))))))))))

#define FMA_D(c, x, rest) FMA_D_LOADED(_mm256_set1_pd(c), x, rest)
#define FMA_D_LOADED(c, x, rest) _mm256_fmadd_pd(x, rest, c)
#define FMA_D_END(c) _mm256_set1_pd(c)

__m256d simd_exp(__m256d x)
{
        const __m256d one = _mm256_set1_pd(1.0);
        // x = _mm256_min_pd(x, _mm256_set1_pd(EXP_HI_F));
        // x = _mm256_max_pd(x, _mm256_set1_pd(EXP_LO_F));
        const __m256d x_clamped = _mm256_max_pd(_mm256_min_pd(x,
                                                             _mm256_set1_pd(EXP_HI_D)),
                                               _mm256_set1_pd(EXP_LO_D));

        const __m256d fx = _mm256_fmadd_pd(x_clamped,
                                          _mm256_set1_pd(LOG2EF_D),
                                          _mm256_set1_pd(0.5));

        const __m256d tmp  = _mm256_cvtepi32_pd(_mm256_cvttpd_epi32(fx));
        /* mask with ones for elements of tmp where tmp[i] > fx[i] */
        const __m256d mask = _mm256_and_pd(_mm256_cmp_pd(tmp, fx, _CMP_GT_OQ), one);
        const __m256d fx_rounded = _mm256_sub_pd(tmp, mask);

        const __m256d x_reduced = _mm256_sub_pd(
                                   _mm256_sub_pd(x,
                                                 _mm256_mul_pd(fx_rounded,
                                                               _mm256_set1_pd(EXP_C1_F))),
                                   _mm256_mul_pd(fx_rounded,
                                                 _mm256_set1_pd(EXP_C2_F)));
        // _mm256_fmsub_pd(fx_rounded,
        //                 _mm256_set1_pd(EXP_C1_F),
        //                 z);

        const __m256d y =
          FMA_D_LOADED(
            one,
            x_reduced,
            FMA_D(EXP_P1_D,
                  x_reduced,
                  FMA_D(EXP_P2_D,
                        x_reduced,
                        FMA_D(EXP_P3_D,
                              x_reduced,
                              FMA_D(EXP_P4_D,
                                    x_reduced,
                                    FMA_D(EXP_P5_D,
                                          x_reduced,
                                          FMA_D(EXP_P6_D,
                                                x_reduced,
                                                FMA_D(EXP_P7_D,
                                                      x_reduced,
                                                      FMA_D(EXP_P8_D,
                                                            x_reduced,
                                                            FMA_D(EXP_P9_D,
                                                                  x_reduced,
                                                                  FMA_D(EXP_P11_D,
                                                                        x_reduced,
                                                                        FMA_D_END(EXP_P10_D))))))))))));

        const __m256i n = _mm256_cvtepi32_epi64(_mm256_cvttpd_epi32(fx_rounded));
        const __m256d pow2n =
          _mm256_castsi256_pd(
            _mm256_slli_epi64(
              _mm256_add_epi64(
                n,
                _mm256_set1_epi64x(0x3ff)), // 1023
              52));
        return _mm256_mul_pd(y, pow2n);
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
