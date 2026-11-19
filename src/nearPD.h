#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#ifndef __NEARPD_H__
#define __NEARPD_H__
#if defined(__cplusplus)

using namespace arma;


// Nearest positive definite matrix.  Returns false when the projection
// fails, and copies the input into `ret` when it does -- so the flag has
// to be honored rather than the distance between the two.
bool rxNearPD(arma::mat &ret, const arma::mat in);

unsigned int rxNearPdChol(Rcpp::NumericMatrix &ret, Rcpp::NumericMatrix x,
                          bool isChol = false);

#define rxNearPdChol_zero 0
#define rxNearPdChol_sympd_chol 1
#define rxNearPdChol_sympd_bad_chol 2
#define rxNearPdChol_nearpd_chol 3
#define rxNearPdChol_nearpd_bad_chol 4
#define rxNearPdChol_bad_nearpd 5
#define rxNearPdChol_zero_size 6
#define rxNearPdChol_not_named 7
#define rxNearPdChol_isChol 8

#endif
#endif
