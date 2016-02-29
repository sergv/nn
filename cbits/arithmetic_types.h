#ifndef ARITHMETIC_TYPES_H_
#define ARITHMETIC_TYPES_H_ 1

/* Created: Friday, 26 February 2016 */

#define FLOAT_ALIGNMENT 32
#define DOUBLE_ALIGNMENT 32
// (sizeof(double) * 4)

typedef float float_aligned __attribute__((aligned (FLOAT_ALIGNMENT)));
typedef double double_aligned __attribute__((aligned (DOUBLE_ALIGNMENT)));

#define FLOAT_ALIGNED __attribute__ ((aligned (FLOAT_ALIGNMENT)))
#define DOUBLE_ALIGNED __attribute__ ((aligned (DOUBLE_ALIGNMENT)))

#endif /* #ifndef ARITHMETIC_TYPES_H_ */
