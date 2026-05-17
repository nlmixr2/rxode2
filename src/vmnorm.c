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
	for (i = 1; i <= n; i++)
		vm = fmax(vm, fabs(v[i]) * w[i]);
	return vm;

}
