#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#include <math.h>

double fnorm0(int n, double **a, double *w)

/*
   This subroutine computes the norm of a full n by n matrix,
   stored in the array a, that is consistent with the weighted max-norm
   on vectors, with weights stored in the array w.

      fnorm = max(i=1,...,n) ( w[i] * sum(j=1,...,n) fabs( a[i][j] ) / w[j] )
*/

{
	int             i, j;
	double          an, sum, *ap1;

	an = 0.;
	for (i = 1; i <= n; i++) {
		sum = 0.;
		ap1 = a[i];
		for (j = 1; j <= n; j++)
			sum += fabs(ap1[j]) / w[j];
		an = fmax(an, sum * w[i]);
	}
	return an;

}
