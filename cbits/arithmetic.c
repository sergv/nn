#include <arithmetic.h>

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
