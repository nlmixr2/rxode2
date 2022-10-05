#define USE_FC_LEN_T
/************
 * idamax.c *
 ************/

#include <math.h>

int idamax0(int n, double *dx, int incx) {

/* Purpose : Find largest component of double vector dx


   --- Input ---

   n    : number of elements in input vector
   dx   : double vector with n+1 elements, dx[0] is not used
   incx : storage spacing between elements of dx


   --- Output ---

   idamax : smallest index, 0 if n <= 0


   Find smallest index of maximum magnitude of dx.
   idamax = first i, i=1 to n, to minimize fabs( dx[1-incx+i*incx] ).

*/

{
	double          dmax, xmag;
	int             i, ii, xindex;

	xindex = 0;
	if (n <= 0)
		return xindex;
	xindex = 1;
	if (n <= 1 || incx <= 0)
		return xindex;

/* Code for increments not equal to 1.   */

	if (incx != 1) {
		dmax = fabs(dx[1]);
		ii = 2;
		for (i = 1 + incx; i <= n * incx; i = i + incx) {
			xmag = fabs(dx[i]);
			if (xmag > dmax) {
				xindex = ii;
				dmax = xmag;
			}
			ii++;
		}
		return xindex;
	}
/* Code for increments equal to 1.  */

	dmax = fabs(dx[1]);
	for (i = 2; i <= n; i++) {
		xmag = fabs(dx[i]);
		if (xmag > dmax) {
			xindex = i;
			dmax = xmag;
		}
	}
	return xindex;

}

}
