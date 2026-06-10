#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rcpp.h>
#include <atomic>

static std::atomic<int> g_cvodeLinSol(1);

extern "C" int getCvodeLinearSolver() {
  return g_cvodeLinSol.load(std::memory_order_relaxed);
}

//' Set the CVODE linear solver
//'
//' @param type Integer: 1=dense (default), 2=band (currently aliases to dense),
//'   3=gmres, 4=bicgstab, 5=tfqmr
//' @return NULL (invisibly)
//' @export
//' @keywords internal
// [[Rcpp::export]]
void setCvodeLinearSolver(int type) {
  g_cvodeLinSol.store(type, std::memory_order_relaxed);
}
