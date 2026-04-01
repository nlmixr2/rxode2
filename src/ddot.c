#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
/**********
 * ddot.c *
 **********/

double ddot0(int n, double *dx, int incx, double *dy, int incy) {

/*
   Purpose : Inner product dx . dy


   --- Input ---

   n    : number of elements in input vector(s)
   dx   : double vector with n+1 elements, dx[0] is not used
   incx : storage spacing between elements of dx
   dy   : double vector with n+1 elements, dy[0] is not used
   incy : storage spacing between elements of dy


   --- Output ---

   ddot : dot product dx . dy, 0 if n <= 0


   ddot = sum for i = 0 to n-1 of
   dx[lx+i*incx] * dy[ly+i*incy] where lx = 1 if
   incx >= 0, else lx = (-incx)*(n-1)+1, and ly
   is defined in a similar way using incy.

*/

{
	double          dotprod;
	int             ix, iy, i; /*, m; */

	dotprod = 0.;
	if (n <= 0)
		return dotprod;

/* Code for unequal or nonpositive increments.  */

	if (incx != incy || incx < 1) {
		ix = 1;
		iy = 1;
		if (incx < 0)
			ix = (-n + 1) * incx + 1;
		if (incy < 0)
			iy = (-n + 1) * incy + 1;
		for (i = 1; i <= n; i++) {
			dotprod = dotprod + dx[ix] * dy[iy];
			ix = ix + incx;
			iy = iy + incy;
		}
		return dotprod;
	}
/* Code for both increments equal to 1.  */

/* Clean-up loop so remaining vector length is a multiple of 5.  */

	if (incx == 1) {
/*
		m = n % 5;
		if (m != 0) {
			for (i = 1; i <= m; i++)
				dotprod = dotprod + dx[i] * dy[i];
			if (n < 5)
				return dotprod;
		}
		for (i = m + 1; i <= n; i = i + 5)
			dotprod = dotprod + dx[i] * dy[i] + dx[i + 1] * dy[i + 1] +
				dx[i + 2] * dy[i + 2] + dx[i + 3] * dy[i + 3] +
				dx[i + 4] * dy[i + 4];
*/
		for (i = 1; i <= n; i++)
			dotprod = dotprod + dx[i] * dy[i];
		return dotprod;
	}
/* Code for positive equal nonunit increments.   */

	for (i = 1; i <= n * incx; i = i + incx)
		dotprod = dotprod + dx[i] * dy[i];
	return dotprod;

}

}
