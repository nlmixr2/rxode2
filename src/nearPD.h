#ifndef __NEARPD_H__
#define __NEARPD_H__
#if defined(__cplusplus)

using namespace arma;


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
