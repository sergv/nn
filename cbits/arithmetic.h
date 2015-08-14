#ifndef ARITHMETIC_H_
#define ARITHMETIC_H_ 1

/* Created: Tuesday, 11 August 2015 */

#define DOUBLE_ALIGNMENT 32
// (sizeof(double) * 4)

typedef double double_aligned __attribute__((aligned(DOUBLE_ALIGNMENT)));
void add(int n,
         const double_aligned * __restrict xs,
         const double_aligned * __restrict ys,
         double_aligned * __restrict zs);

/* zs = xs + c * ys */
void addScaled(int n,
               const double_aligned * __restrict xs,
               double c,
               const double_aligned * __restrict ys,
               double_aligned * __restrict zs);

double dot(int n,
           const double_aligned * __restrict xs,
           const double_aligned * __restrict ys);


#endif /* #ifndef ARITHMETIC_H_ */
