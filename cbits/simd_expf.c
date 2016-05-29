#include <arithmetic_types.h>
#include <simd_expf.h>

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

__attribute__((always_inline))
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
                                   _mm256_sub_ps(x_clamped,
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

        const __m256 pow2n =
          _mm256_castsi256_ps(
            _mm256_slli_epi32(
              _mm256_add_epi32(
                _mm256_cvttps_epi32(fx_rounded),
                _mm256_set1_epi32(0x7f)), // 127
              23));

        return _mm256_mul_ps(y, pow2n);
}
