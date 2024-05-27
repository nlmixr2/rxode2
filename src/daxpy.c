#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
/***********
 * daxpy.c *
 ***********/

/*
From tam@dragonfly.wri.com Wed Apr 24 15:48:31 1991
Return-Path: <tam>
Date: Wed, 24 Apr 91 17:48:43 CDT
From: tam@dragonfly.wri.com
To: whitbeck@sanjuan.wrc.unr.edu
*/

void daxpy0(int n, double da, double *dx, int incx, double *dy, int incy) {

/*
   Purpose : To compute

   dy = da * dx + dy


   --- Input ---

   n    : number of elements in input vector(s)
   da   : double scalar multiplier
   dx   : double vector with n+1 elements, dx[0] is not used
   incx : storage spacing between elements of dx
   dy   : double vector with n+1 elements, dy[0] is not used
   incy : storage spacing between elements of dy


   --- Output ---

   dy = da * dx + dy, unchanged if n <= 0


   For i = 0 to n-1, replace dy[ly+i*incy] with
   da*dx[lx+i*incx] + dy[ly+i*incy], where lx = 1
   if  incx >= 0, else lx = (-incx)*(n-1)+1 and ly is
   defined in a similar way using incy.

*/

{
	int             ix, iy, i, m;

	if (n < 0 || da == 0.)
		return;

/* Code for nonequal or nonpositive increments.  */

	if (incx != incy || incx < 1) {
		ix = 1;
		iy = 1;
		if (incx < 0)
			ix = (-n + 1) * incx + 1;
		if (incy < 0)
			iy = (-n + 1) * incy + 1;
		for (i = 1; i <= n; i++) {
			dy[iy] = dy[iy] + da * dx[ix];
			ix = ix + incx;
			iy = iy + incy;
		}
		return;
	}
/* Code for both increments equal to 1.   */

/* Clean-up loop so remaining vector length is a multiple of 4.  */

	if (incx == 1) {
		m = n % 4;
		if (m != 0) {
			for (i = 1; i <= m; i++)
				dy[i] = dy[i] + da * dx[i];
			if (n < 4)
				return;
		}
		for (i = m + 1; i <= n; i = i + 4) {
			dy[i] = dy[i] + da * dx[i];
			dy[i + 1] = dy[i + 1] + da * dx[i + 1];
			dy[i + 2] = dy[i + 2] + da * dx[i + 2];
			dy[i + 3] = dy[i + 3] + da * dx[i + 3];
		}
		return;
	}
/* Code for equal, positive, nonunit increments.   */

	for (i = 1; i <= n * incx; i = i + incx)
		dy[i] = da * dx[i] + dy[i];
	return;

}

}
