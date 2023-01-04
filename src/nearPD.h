#ifndef __NEARPD_H__
#define __NEARPD_H__
#if defined(__cplusplus)

using namespace arma;

#define nmNearPD rxNearPD
#define rx_chol_sym chol_sym
#define rx_inv_sym inv_sym
#define nmRepEach rxRepEach
#define nmMatVecSameLen rxMatVecSameLen
#define nmPmaxC rxPmaxC
#define eig_symR rx_eig_symR
#define eig_sym2 rx_eig_sym2

bool rxNearPD(mat &ret, mat x, bool keepDiag = true,
             bool do2eigen = true, bool doDykstra = true, bool only_values = false,
             double eig_tol   = 1e-6, double conv_tol  = 1e-7, double posd_tol  = 1e-8,
             int maxit    = 1000, bool trace = false // set to TRUE (or 1 ..) to trace iterations
             );

unsigned int rxNearPdChol(Rcpp::NumericMatrix &ret, Rcpp::NumericMatrix x,
                          bool isChol = false,
                          bool keepDia = false
                          , bool do2eigen = true  // if TRUE do a sfsmisc::posdefify() eigen step
                          , bool doDykstra = true // do use Dykstra's correction
                          , bool only_values = false // if TRUE simply return lambda[j].
                          , double eig_tol   = 1e-6 // defines relative positiveness of eigenvalues compared to largest
                          , double conv_tol  = 1e-7 // convergence tolerance for algorithm
                          , double posd_tol  = 1e-8 // tolerance for enforcing positive definiteness
                          , int maxit    = 100 // maximum number of iterations allowed
                          , bool trace = false // set to TRUE (or 1 ..) to trace iterations) {
                          );

bool chol_sym(mat &Hout, mat& Hin);
bool inv_sym(mat &Hout, mat& Hin);

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
