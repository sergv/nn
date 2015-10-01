#ifndef ARITHMETIC_H_
#define ARITHMETIC_H_ 1

/* Created: Tuesday, 11 August 2015 */

#define FLOAT_ALIGNMENT 32
#define DOUBLE_ALIGNMENT 32
// (sizeof(double) * 4)

typedef float float_aligned __attribute__((aligned(FLOAT_ALIGNMENT)));
typedef double double_aligned __attribute__((aligned(DOUBLE_ALIGNMENT)));

void addf(unsigned int n,
          const float_aligned * __restrict xs,
          const float_aligned * __restrict ys,
          float_aligned * __restrict zs);
void add(unsigned int n,
         const double_aligned * __restrict xs,
         const double_aligned * __restrict ys,
         double_aligned * __restrict zs);

/* zs = xs + c * ys */

void addScaledf(unsigned int n,
                const float_aligned * __restrict xs,
                float c,
                const float_aligned * __restrict ys,
                float_aligned * __restrict zs);
void addScaled(unsigned int n,
               const double_aligned * __restrict xs,
               double c,
               const double_aligned * __restrict ys,
               double_aligned * __restrict zs);

float dotf(unsigned int n,
           const float_aligned * __restrict xs,
           const float_aligned * __restrict ys);
double dot(unsigned int n,
           const double_aligned * __restrict xs,
           const double_aligned * __restrict ys);

void map_expf(unsigned int n,
              const float_aligned * __restrict xs,
              float_aligned * __restrict ys);
void map_sigmoid_with_derivf(unsigned int n,
                             const float_aligned * __restrict xs,
                             float_aligned * __restrict ys,
                             float_aligned * __restrict zs);
void map_tanh_with_derivf(unsigned int n,
                          const float_aligned * __restrict xs,
                          float_aligned * __restrict ys,
                          float_aligned * __restrict zs);

void map_exp(unsigned int n,
             const double_aligned * __restrict xs,
             double_aligned * __restrict ys);
void map_sigmoid_with_deriv(unsigned int n,
                            const double_aligned * __restrict xs,
                            double_aligned * __restrict ys,
                            double_aligned * __restrict zs);
void map_tanh_with_deriv(unsigned int n,
                         const double_aligned * __restrict xs,
                         double_aligned * __restrict ys,
                         double_aligned * __restrict zs);

#endif /* #ifndef ARITHMETIC_H_ */
