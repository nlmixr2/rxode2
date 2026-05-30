#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rcpp.h>

// Global controlling which CVODE linear solver is used (1=dense, 2=band,
// 3=gmres, 4=bicgstab, 5=tfqmr).  Referenced as extern in cvode.cpp.
int g_cvodeLinSol = 1;

//' Set the CVODE linear solver
//'
//' @param type Integer: 1=dense (default), 2=band, 3=gmres, 4=bicgstab,
//'   5=tfqmr
//' @return NULL (invisibly)
//' @export
//' @keywords internal
// [[Rcpp::export]]
void setCvodeLinearSolver(int type) {
  g_cvodeLinSol = type;
}
