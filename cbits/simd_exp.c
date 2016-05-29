#include <arithmetic_types.h>
#include <simd_exp.h>

#define EXP_HI_D 7.08396418532264106224E2     /* log (2**1022) */
#define EXP_LO_D -7.08396418532264106224E2    /* log (2**-1022) */

#define EXP_C1_D 6.93145751953125E-1
#define EXP_C2_D 1.42860682030941723212E-6
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

__attribute__((always_inline))
__m256d simd_exp(__m256d x)
{
        const __m256d one = _mm256_set1_pd(1.0);
        // x = _mm256_min_pd(x, _mm256_set1_pd(EXP_HI_D));
        // x = _mm256_max_pd(x, _mm256_set1_pd(EXP_LO_D));
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
                                   _mm256_sub_pd(x_clamped,
                                                 _mm256_mul_pd(fx_rounded,
                                                               _mm256_set1_pd(EXP_C1_D))),
                                   _mm256_mul_pd(fx_rounded,
                                                 _mm256_set1_pd(EXP_C2_D)));
        // _mm256_fmsub_pd(fx_rounded,
        //                 _mm256_set1_pd(EXP_C1_D),
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
