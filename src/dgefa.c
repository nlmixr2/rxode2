#define USE_FC_LEN_T
#include "blas.h"

/***********
 * dgefa.c *
 ***********/

void dgefa0(double **a, int n, int *ipvt, int *info) {

  /*
    Purpose : dgefa factors a double matrix by Gaussian elimination.

    dgefa is usually called by dgeco, but it can be called directly
    with a saving in time if rcond is not needed.
    (Time for dgeco) = (1+9/n)*(time for dgefa).

    This c version uses algorithm kji rather than the kij in dgefa.f.
    Note that the fortran version input variable lda is not needed.


    On Entry :

    a   : double matrix of dimension ( n+1, n+1 ),
    the 0-th row and column are not used.
    a is created using NewDoubleMatrix, hence
    lda is unnecessary.
    n   : the row dimension of a.

    On Return :

    a     : a lower triangular matrix and the multipliers
    which were used to obtain it.  The factorization
    can be written a = L * U where U is a product of
    permutation and unit upper triangular matrices
    and L is lower triangular.
    ipvt  : an n+1 integer vector of pivot indices.
    *info : = 0 normal value,
    = k if U[k][k] == 0.  This is not an error
    condition for this subroutine, but it does
    indicate that dgesl or dgedi will divide by
    zero if called.  Use rcond in dgeco for
    a reliable indication of singularity.

    Notice that the calling program must use &info.

    BLAS : daxpy, dscal, idamax
  */

  {
	int             j, k, i;
	double          t;

    /* Gaussian elimination with partial pivoting.   */

	*info = 0;
	for (k = 1; k <= n - 1; k++) {
      /*
        Find j = pivot index.  Note that a[k]+k-1 is the address of
        the 0-th element of the row vector whose 1st element is a[k][k].
      */
      j = idamax0(n - k + 1, a[k] + k - 1, 1) + k - 1;
      ipvt[k] = j;
      /*
        Zero pivot implies this row already triangularized.
      */
      if (a[k][j] == 0.) {
        *info = k;
        continue;
      }
      /*
        Interchange if necessary.
      */
      if (j != k) {
        t = a[k][j];
        a[k][j] = a[k][k];
        a[k][k] = t;
      }
      /*
        Compute multipliers.
      */
      t = -1. / a[k][k];
      dscal0(n - k, t, a[k] + k, 1);
      /*
        Column elimination with row indexing.
      */
      for (i = k + 1; i <= n; i++) {
        t = a[i][j];
        if (j != k) {
          a[i][j] = a[i][k];
          a[i][k] = t;
        }
        daxpy0(n - k, t, a[k] + k, 1, a[i] + k, 1);
      }
	}			/* end k-loop  */

	ipvt[n] = n;
	if (a[n][n] == 0.)
      *info = n;

  }
}
