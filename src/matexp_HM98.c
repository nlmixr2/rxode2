#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
// Taken from expm::expm; Its not exported as a C call.
/* Copyright (C) 2013-2014 Drew Schmidt.
   Copyright (C) 2014      Martin Maechler

  This program is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation; either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with
  this program; if not, see <http://www.gnu.org/licenses/>.
*/


/* Matrix exponentiation algorithm from:
   "New Scaling and Squaring Algorithm for the Matrix Exponential", by
   Awad H. Al-Mohy and Nicholas J. Higham, August 2009
*/
#define STRICT_R_HEADERS
#include <stdlib.h>
// #include <assert.h>
#include <math.h>

#include <Rconfig.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Lapack.h>
#include <R_ext/BLAS.h>
#ifndef FCONE
# define FCONE
#endif



#define SGNEXP(x,pow) (x==0?(pow==0?1:0):(x>0?1:(pow%2==0?1:(-1))))

// --------------------------------------------------------
// Utilities
// --------------------------------------------------------

// C = A * B for square matrices
static inline void matprod(int n, double *A, double *B, double *C)
{
    const double one = 1.0, zero = 0.0;
    F77_CALL(dgemm)("N", "N", &n, &n, &n, &one, A, &n, B, &n, &zero, C, &n FCONE FCONE);
}



// Copy A ONTO B, i.e. B = A
static inline void matcopy(int n, double *A, double *B)
{
  F77_CALL(dlacpy)("A", &n, &n, A, &n, B, &n FCONE);
}



/** Identity matrix
 *
 * @param n  integer >= 1
 * @param a  n x n pre-allocated to contain the identity matrix
 */
static inline void mateye(const unsigned int n, double *a)
{
  int i;

  for (i=0; i<n*n; i++)
    a[i] = 0.0;

  i = 0;
  while (i < n*n)
  {
    a[i] = 1.0;
    i += n+1;
  }
}



// 1-norm for a square matrix
static double matnorm_1(const double *x, const int n)
{
  double norm = 0; // norm := max(colSums(abs(x)))
  for (int j=0; j<n; j++) {
      double tmp = 0;
      for (int i=0; i<n; i++)
	  tmp += fabs(x[i + j*n]);
      if (tmp > norm)
	  norm = tmp;
  }
  return norm;
}


#define NTHETA 5

/* Al-Mohy & Higham (2009), Table 2.1: theta[i] is the largest ||A||_1 for which
   Pade degree degs[i] is accurate to double precision.  The degree and the
   scaling MUST be chosen together -- each threshold belongs to one degree.

   This previously returned only the scaling and left the degree to the caller's
   fixed `p` (rxSolve(indLinMatExpOrder=), default 6), so a matrix with
   ||A||_1 anywhere up to theta[4] = 5.37 was evaluated at degree 6 with no
   scaling at all, when the table says that norm needs degree 13.  The result
   was silently wrong rather than an error: on a linear two-compartment model
   it delivered 1.8e-6 where every other kernel delivered 5.8e-11, and under an
   exponential Rosenbrock step it could make the error estimate unsatisfiable,
   so the controller shrank the step without limit -- one van der Pol subject at
   mu = 95.7866 ran for over 390 s against 0.03 s for the other kernels.

   Picking both from the norm is what `matrixExpTaylor` already does, and it is
   why `indLinMatExpOrder` no longer applies to this kernel. */
static int matexp_deg_scale(const double *x, const int n, int *deg)
{
    const double theta[NTHETA] = {1.495585217958292e-2, 2.539398330063230e-1,
                                  9.504178996162932e-1, 2.097847961257068e0,
                                  5.371920351148152e0};
    const int degs[NTHETA] = {3, 5, 7, 9, 13};
    const double x_1 = matnorm_1(x, n);

    if (!R_FINITE(x_1)) {   /* nothing sensible to scale by; stay at the top
                               degree and let the caller see the result */
        *deg = degs[NTHETA-1];
        return 0;
    }
    for (int i = 0; i < NTHETA; i++) {
        if (x_1 <= theta[i]) {
            *deg = degs[i];
            return 0;
        }
    }
    *deg = degs[NTHETA-1];
    int i = (int) ceil(log2(x_1/theta[NTHETA-1]));
    if (i < 0) i = 0;
    if (i > 30) i = 30;     /* 1 << 31 is undefined; 2^30 squarings is already
                               far past any operand this solver can produce */
    return 1 << i;
}

// ___ MM: FIXME  we have a  matpow() already in  ./matpow.c
//     --- Merge the two, keep the better one

// Matrix power by squaring: P = A^b (A is garbage on exit)
// `wsp` is n*n scratch supplied by the caller -- see matexp_MH09().
static void matpow_by_squaring(double *A, int n, int b, double *P, double *wsp)
{
    if (b == 1) {
	matcopy(n, A, P);
	return;
    }
    mateye(n, P);  // P := I
    if (b == 0)
	return;

    // General case: b >= 2
    double *TMP = wsp;

    while (b) {
	if (b&1) { // P := P A
	    matprod(n, P, A, TMP);
	    matcopy(n, TMP, P);
	}

	b >>= 1;
	// A := A^2 :
	matprod(n, A, A, TMP);
	matcopy(n, TMP, A);
    }
}


// --------------------------------------------------------
// Matrix Exponentiation via Pade' Approximations
// --------------------------------------------------------

const double matexp_pade_coefs[14] =
{
  1.0,
  0.5,
  0.12,
  1.833333333333333333333e-2,
  1.992753623188405797101e-3,
  1.630434782608695652174e-4,
  1.035196687370600414079e-5,
  5.175983436853002070393e-7,
  2.043151356652500817261e-8,
  6.306022705717595115002e-10,
  1.483770048404140027059e-11,
  2.529153491597965955215e-13,
  2.810170546219962172461e-15,
  1.544049750670308885967e-17
};



/* r_m(x) = p_m(x) / q_m(x), where
   p_m(x) = sum_{j=0}^m (2m-j)!m!/(2m)!/(m-j)!/j! * x^j

   and q_m(x) = p_m(-x)
*/

// Workhorse for matexp_pade
void matexp_pade_fillmats(const int m, const int n, const int i,
			  double *N, double *D, double *B, double *C)
{
  const double tmp = matexp_pade_coefs[i];
  const int sgn = SGNEXP(-1, i);

    /* Performs the following actions:
        B = C
        N = pade_coef[i] * C
        D = (-1)^j * pade_coef[i] * C
    */
    for (int j=0; j < m*n; j++) {
	double t_j = C[j]; B[j] = t_j;
	t_j *= tmp;
	N[j] +=     t_j;
	D[j] += sgn*t_j;
    }
}



/**
 * Exponentiation via Pade' expansion
 *
 * @param n
 * @param p
 * @param A
 * @param N
 */
// `wsp` is 3*n*n doubles and `iwsp` n ints of caller-supplied scratch.
static void matexp_pade(int n, const int p, double *A, double *N,
                        double *wsp, int *iwsp)
{
    int i, info = 0, n2 = n*n;
    // FIXME: check n2 (or n, such that n2 did not overflow !)

    // Power of A
    double *B = wsp;

    // Temporary storage for matrix multiplication;  matcopy(n, A, C);
    double *C = Memcpy(wsp + n2, A, n2);

    double *D = wsp + 2*n2;

    for (i=0; i<n*n; i++) {
	N[i] = 0.0;
	D[i] = 0.0;
    }

    i = 0;
    while (i < n*n) {
	N[i] = 1.0;
	D[i] = 1.0;

	i += n+1;
    }


    // Fill N and D
    for (i=1; i<=p; i++)
    {
	// C = A*B
	if (i > 1)
	    matprod(n, A, B, C);

	// Update matrices
	matexp_pade_fillmats(n, n, i, N, D, B, C);
    }

    // R <- inverse(D) %*% N
    int *ipiv = iwsp;

    F77_CALL(dgesv)(&n, &n, D, &n, ipiv, N, &n, &info);

} // matexp_pade()


/**
 * Matrix Exponential
 *
 * @param x Input (square) matrix.  On exit, the values in x are "garbage"!
 * @param n Number of rows/cols of (square) matrix x.
 * @param p Order of the Pade' approximation. 0 < p <= 13.
 * @param ret On exit, ret = expm(x).
 */
/* Scratch is owned here rather than taken from R's transient vmax stack.
   This is reached from expm.cpp inside indLin()'s omp parallel region, and
   R_alloc uses a single unlocked global that is not released until the
   enclosing .Call returns -- so the old code both raced between threads and
   grew vmax monotonically across a solve.  Compartmental models are small
   enough to stay on the stack; anything larger falls back to malloc, which
   is thread-safe. */
#define MATEXP_STACK_N 12

void matexp_MH09(double *x, int n, const int p, double *ret)
{
  int nn = n*n;
  double wspStack[3*MATEXP_STACK_N*MATEXP_STACK_N];
  int iwspStack[MATEXP_STACK_N];
  double *wsp = wspStack;
  int *iwsp = iwspStack;
  double *wspHeap = NULL;
  int *iwspHeap = NULL;
  if (n > MATEXP_STACK_N) {
    wspHeap = (double *) malloc(3*(size_t)nn*sizeof(double));
    iwspHeap = (int *) malloc((size_t)n*sizeof(int));
    if (wspHeap == NULL || iwspHeap == NULL) {
      /* Never longjmp from here: a worker thread cannot unwind past the
         parallel region.  Report zeros and let the caller notice. */
      free(wspHeap);
      free(iwspHeap);
      for (int i = 0; i < nn; i++) ret[i] = 0.0;
      return;
    }
    wsp = wspHeap;
    iwsp = iwspHeap;
  }

  /* `p` is deliberately unused: the degree is not free to choose, it is fixed
     by the norm together with the scaling.  See matexp_deg_scale(). */
  (void) p;
  int deg = 13;
  int m = matexp_deg_scale(x, n, &deg);

  if (m == 0) {
      matexp_pade(n, deg, x, ret, wsp, iwsp);
  } else {
    int one = 1;
    double tmp = 1. / ((double) m);

    F77_CALL(dscal)(&nn, &tmp, x, &one);

    matexp_pade(n, deg, x, ret, wsp, iwsp);

    matcopy(n, ret, x);

    /* matexp_pade() has returned, so its scratch is free to reuse here. */
    matpow_by_squaring(x, n, m, ret, wsp);
  }

  free(wspHeap);
  free(iwspHeap);
}
