#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#include "blas.h"
/***********
 * dgesl.c *
 ***********/

void dgesl0(double **a, int n, int *ipvt, double *b, int job) {

  /*
    Purpose : dgesl solves the linear system
    a * x = b or Transpose(a) * x = b
    using the factors computed by dgeco or degfa.


    On Entry :

    a    : double matrix of dimension ( n+1, n+1 ),
    the output from dgeco or dgefa.
    The 0-th row and column are not used.
    n    : the row dimension of a.
    ipvt : the pivot vector from degco or dgefa.
    b    : the right hand side vector.
    job  : = 0       to solve a * x = b,
    = nonzero to solve Transpose(a) * x = b.


    On Return :

    b : the solution vector x.


    Error Condition :

    A division by zero will occur if the input factor contains
    a zero on the diagonal.  Technically this indicates
    singularity but it is often caused by improper argments or
    improper setting of the pointers of a.  It will not occur
    if the subroutines are called correctly and if dgeco has
    set rcond > 0 or dgefa has set info = 0.


    BLAS : daxpy, ddot
  */

  {
	int             k, j;
	double          t;

	/* int nm1 = n - 1; */

    /*
      Job = 0, solve a * x = b.
    */
	if (job == 0) {
      /*
        First solve L * y = b.
      */
      for (k = 1; k <= n; k++) {
        t = ddot0(k - 1, a[k], 1, b, 1);
        b[k] = (b[k] - t) / a[k][k];
      }
      /*
        Now solve U * x = y.
      */
      for (k = n - 1; k >= 1; k--) {
        b[k] = b[k] + ddot0(n - k, a[k] + k, 1, b + k, 1);
        j = ipvt[k];
        if (j != k) {
          t = b[j];
          b[j] = b[k];
          b[k] = t;
        }
      }
      return;
	}
    /*
      Job = nonzero, solve Transpose(a) * x = b.

      First solve Transpose(U) * y = b.
    */
	for (k = 1; k <= n - 1; k++) {
      j = ipvt[k];
      t = b[j];
      if (j != k) {
        b[j] = b[k];
        b[k] = t;
      }
      daxpy0(n - k, t, a[k] + k, 1, b + k, 1);
	}
    /*
      Now solve Transpose(L) * x = y.
    */
	for (k = n; k >= 1; k--) {
      b[k] = b[k] / a[k][k];
      t = -b[k];
      daxpy0(k - 1, t, a[k], 1, b, 1);
	}

  }

}
