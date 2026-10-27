// Truncated normal helpers shared by the truncated multivariate normal
// (rxthreefry.cpp) and truncated multivariate t (rxRmvt.cpp) samplers.
#ifndef __RXTRUNC_H__
#define __RXTRUNC_H__

#include <RcppArmadillo.h>
#include <threefry.h>

typedef struct {
  arma::mat L;
  arma::vec l;
  arma::vec u;
  arma::uvec perm;
} rx_cholperms;

double trandn(double l, double u, sitmo::threefry& eng, double a, double tol);
double lnNpr(double a, double b);
// Permuted Cholesky factor; status is 0 on success, 1 when Sig is not PSD.
// Does not call the R API, so it may run on any thread.
rx_cholperms cholpermCore(arma::mat Sig, const arma::vec& cl, const arma::vec& cu,
                          double eps, int& status);

// Writes y (original order) from the standardized z: y[perm[j]] = scale*(Lfull z)[j].
static inline void rxTruncBackTransform(const arma::mat& Lfull, const double* z,
                                        const arma::uvec& perm, double scale,
                                        double* y) {
  int d = Lfull.n_rows;
  for (int j = 0; j < d; ++j) {
    double acc = 0.0;
    for (int i = 0; i <= j; ++i) acc += Lfull.at(j, i) * z[i];
    y[perm[j]] = scale * acc;
  }
}

// Truncated/untruncated multivariate t for the R interface (rxRmvt.cpp); fills
// the n x d matrix A from one serially seeded engine; errors with Rcpp::stop().
void rxRmvt0(arma::mat& A, const arma::rowvec& mu, const arma::mat& sigma,
             const arma::vec& lower, const arma::vec& upper, double df,
             bool isChol, double a, double tol, double nlTol, int nlMaxiter);

#endif
