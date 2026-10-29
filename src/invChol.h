#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#ifndef __INVCHOL_H__
#define __INVCHOL_H__
#if defined(__cplusplus)

// Inverse Cholesky parameterization of a covariance matrix `omega`:
// `inv(omega) = R' R` with `R` upper triangular.  The parameters follow
// the column-major lower triangle of `omega`; entry (i, j), i > j, is
// `R(j, i)` and entry (i, i) is `log(R(i, i))`, so every real vector maps
// back to a positive definite matrix.

// Position of entry (i, j) of a `dim` x `dim` matrix in that order
int rxInvCholIdx(int i, int j, int dim);

// The parameters of `omega`; false when it is not positive definite
bool rxInvCholPar(arma::vec &par, const arma::mat &omega);

// The matrix the parameters describe
arma::mat rxInvCholMat(const arma::vec &par, int dim);

// d(lower triangle of omega)/d(par) at `omega`; false when it is not
// positive definite
bool rxInvCholJac(arma::mat &jac, const arma::mat &omega);

#endif
#endif
