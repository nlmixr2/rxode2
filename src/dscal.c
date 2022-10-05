#define USE_FC_LEN_T
/***********
 * dscal.c *
 ***********/

void dscal0(int n, double da, double *dx, int incx) {

/* Purpose : scalar vector multiplication

   dx = da * dx


   --- Input ---

   n    : number of elements in input vector
   da   : double scale factor
   dx   : double vector with n+1 elements, dx[0] is not used
   incx : storage spacing between elements of dx


   --- Output ---

   dx = da * dx, unchanged if n <= 0


   For i = 0 to n-1, replace dx[1+i*incx] with
   da * dx[1+i*incx].

*/

{
	int             m, i;

	if (n <= 0)
		return;

/* Code for increments not equal to 1.  */

	if (incx != 1) {
		for (i = 1; i <= n * incx; i = i + incx)
			dx[i] = da * dx[i];
		return;
	}
/* Code for increments equal to 1.  */

/* Clean-up loop so remaining vector length is a multiple of 5.  */

	m = n % 5;
	if (m != 0) {
		for (i = 1; i <= m; i++)
			dx[i] = da * dx[i];
		if (n < 5)
			return;
	}
	for (i = m + 1; i <= n; i = i + 5) {
		dx[i] = da * dx[i];
		dx[i + 1] = da * dx[i + 1];
		dx[i + 2] = da * dx[i + 2];
		dx[i + 3] = da * dx[i + 3];
		dx[i + 4] = da * dx[i + 4];
	}
	return;

}

}
