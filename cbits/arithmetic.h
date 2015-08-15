#ifndef ARITHMETIC_H_
#define ARITHMETIC_H_ 1

/* Created: Tuesday, 11 August 2015 */

#define FLOAT_ALIGNMENT 32
#define DOUBLE_ALIGNMENT 32
// (sizeof(double) * 4)

typedef float float_aligned __attribute__((aligned(FLOAT_ALIGNMENT)));
typedef double double_aligned __attribute__((aligned(DOUBLE_ALIGNMENT)));


void addf(int n,
          const float_aligned * __restrict xs,
          const float_aligned * __restrict ys,
          float_aligned * __restrict zs);
void add(int n,
         const double_aligned * __restrict xs,
         const double_aligned * __restrict ys,
         double_aligned * __restrict zs);

/* zs = xs + c * ys */

void addScaledf(int n,
                const float_aligned * __restrict xs,
                float c,
                const float_aligned * __restrict ys,
                float_aligned * __restrict zs);
void addScaled(int n,
               const double_aligned * __restrict xs,
               double c,
               const double_aligned * __restrict ys,
               double_aligned * __restrict zs);

float dotf(int n,
           const float_aligned * __restrict xs,
           const float_aligned * __restrict ys);
double dot(int n,
           const double_aligned * __restrict xs,
           const double_aligned * __restrict ys);

#endif /* #ifndef ARITHMETIC_H_ */
