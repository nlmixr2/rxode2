#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rcpp.h>
#include <atomic>

static std::atomic<int> g_cvodeLinSol(1);

extern "C" int getCvodeLinearSolver() {
  return g_cvodeLinSol.load(std::memory_order_relaxed);
}

// Lower/upper Jacobian bandwidth for the "band" linear solver; -1 = unknown
// (band then aliases to dense).  Set by rxSolve() from the model's state
// dependencies.
static std::atomic<int> g_cvodeBandMl(-1);
static std::atomic<int> g_cvodeBandMu(-1);

extern "C" int getCvodeBandMl() {
  return g_cvodeBandMl.load(std::memory_order_relaxed);
}

extern "C" int getCvodeBandMu() {
  return g_cvodeBandMu.load(std::memory_order_relaxed);
}

// Returns the previous c(ml, mu) so the caller can restore it.
extern "C" SEXP _rxode2_setCvodeBand(SEXP ml, SEXP mu) {
  Rcpp::IntegerVector ret = Rcpp::IntegerVector::create(getCvodeBandMl(), getCvodeBandMu());
  g_cvodeBandMl.store(Rf_asInteger(ml), std::memory_order_relaxed);
  g_cvodeBandMu.store(Rf_asInteger(mu), std::memory_order_relaxed);
  return ret;
}

//' Set the CVODE linear solver
//'
//' @param type Integer: 1=dense (default), 2=band (dense when the bandwidth is unknown),
//'   3=gmres, 4=bicgstab, 5=tfqmr
//' @return NULL (invisibly)
//' @export
//' @keywords internal
// [[Rcpp::export]]
void setCvodeLinearSolver(int type) {
  g_cvodeLinSol.store(type, std::memory_order_relaxed);
}
