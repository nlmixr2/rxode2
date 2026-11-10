#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#ifndef __INVCHOL_H__
#define __INVCHOL_H__
#if defined(__cplusplus)
#include <vector>

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

// The joint draw of `omegaSeparation="invchol"`: the entries are drawn as
// parameters of `rxInvCholPar()` so the matrix is always positive definite.
// Only the blocks the drawn entries touch are moved.
struct rxInvCholJoint {
  bool on = false;
  arma::uvec dims;          // rows/columns of the matrix that are moved
  arma::vec base;           // parameters of the estimate's `dims` submatrix
  std::vector<int> parIdx;  // per entry, its parameter (-1 when not drawn)
};

// Set up `ic` from the estimate `mat` and the entries `el` (thetaMat
// column `col`), moving the thetaMat covariance `cov` onto the parameter
// scale; false when the estimate is not positive definite
bool rxInvCholJointSetup(rxInvCholJoint &ic, arma::mat &cov,
                         const arma::mat &mat, const Rcpp::IntegerMatrix &el,
                         const std::vector<int> &col);

// `cur` with the drawn entries moved by the deviation `dev`
arma::mat rxInvCholJointCand(const rxInvCholJoint &ic, const arma::mat &cur,
                             const Rcpp::NumericVector &dev,
                             const std::vector<int> &col);

#endif
#endif
