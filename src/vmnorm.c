#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#include <math.h>

double vmnorm0(int n, double *v, double *w)

/*
   This function routine computes the weighted max-norm
   of the vector of length n contained in the array v, with weights
   contained in the array w of length n.

   vmnorm = max( i = 1, ..., n ) fabs( v[i] ) * w[i].
*/

{
	int             i;
	double          vm;

	vm = 0.;
	for (i = 1; i <= n; i++) {
		double x = fabs(v[i]) * w[i];
		/* the larger of vm and x, as fmax(vm, x) gives it (a NaN x leaves
		   vm), without a libm call per element */
		if (x > vm) vm = x;
	}
	return vm;

}
