// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
// [[Rcpp::interfaces(r,cpp)]]
#include <RcppArmadillo.h>
#include "../inst/include/rxode2.h"       /* rxLlikSaveSize; pulls in rxode2parseStruct.h */
#include "../inst/include/rxMemoryCalc.h" /* rx_mem_layout, rxFillMemLayout()             */

using namespace Rcpp;

//' Report the byte sizes of every rxode2 solver memory allocation
//'
//' Returns a named numeric vector of byte counts for every buffer rxode2
//' allocates during \code{rxSolve()}.  Values are computed with the same
//' formulas used by the actual allocator in \code{rxData.cpp} via the shared
//' \code{rxFillMemLayout()} function, so any change to the allocator
//' automatically changes the estimate.
//'
//' @param neq       Number of ODE states (\code{length(rxModelVars(model)$state)}).
//' @param stateSize Effective \code{state.size()} passed to the solver.
//'   For pure ODE models this equals \code{neq}; for linCmt-only models it
//'   may be 0.  Use \code{neq} when in doubt.
//' @param nlhs      Number of LHS (calculated) outputs.
//' @param npars     Number of model parameters (for \code{gpars} estimate).
//' @param neta      Number of random effects (etas).
//' @param neps      Number of residual-error levels (epsilons).
//' @param ncov      Number of time-varying covariates.
//' @param nsim      Number of simulations.
//' @param cores     Number of parallel OMP threads.
//' @param nMtime    Number of model measurement times.
//' @param extraCmt  Extra compartments (0, 1 = depot, 2 = depot+central).
//' @param linB      1 if using an analytical linear-compartment model, else 0.
//' @param nLlik     Number of log-likelihood terms (FOCEi).
//' @param nIndSim   Per-individual simulation count (typically \code{neta+neps}).
//' @param numLinSens Number of linear sensitivity parameters (FOCEi mixed models).
//' @param numLin    Number of linear compartment terms (FOCEi mixed models).
//' @param nsub      Number of subjects.
//' @param nallTotal Total events across all subjects (sum of obs + doses).
//' @param maxAllTimes Maximum events for any single subject.
//' @return Named numeric vector; each element is bytes for that allocation.
//'   Also includes \code{sizeofInd} (bytes per \code{rx_solving_options_ind}
//'   struct) and \code{rxLlikSaveSize} (the compile-time constant).
//' @export
// [[Rcpp::export]]
NumericVector rxMemoryComponents_(
  int    neq,
  int    stateSize,
  int    nlhs,
  int    npars,
  int    neta,
  int    neps,
  int    ncov,
  int    nsim,
  int    cores,
  int    nMtime,
  int    extraCmt,
  int    linB,
  int    nLlik,
  int    nIndSim,
  int    numLinSens,
  int    numLin,
  int    nsub,
  double nallTotal,
  double maxAllTimes)
{
  rx_mem_layout _mem;
  rxFillMemLayout(
    neq,
    stateSize,
    nlhs,
    nsim,
    cores,
    nMtime,
    extraCmt,
    linB,
    nLlik,
    nIndSim,
    nsub,
    (int64_t)nallTotal,
    (int)maxAllTimes,
    numLinSens,
    numLin,
    (int64_t)neq,          /* n4_actual: neq proxy for estimate */
    (int64_t)neq,          /* n6_actual: neq proxy for estimate */
    &_mem);

  /* Byte counts for allocations outside gsolve/gon */
  double b_gall_times  = 5.0  * nallTotal * sizeof(double);
  double b_gevid       = 3.0  * nallTotal * sizeof(int);
  double b_gcov        = (double)ncov * nallTotal * sizeof(double);
  double b_gpars       = (double)npars * nsub * sizeof(double);
  double b_gomega      = (double)(2 * neta + neta * neta) * sizeof(double);
  double b_gsigma      = (double)(2 * neps + neps * neps) * sizeof(double);
  double b_gall_timesS = (nsim > 1)
                           ? 2.0 * (nsim - 1) * nallTotal * sizeof(double)
                           : 0.0;
  double b_ordId       = nallTotal * sizeof(int);
  double b_gInfRate    = (double)cores * (neq + extraCmt) * sizeof(double);
  double b_inds        = (double)nsub  * sizeof(rx_solving_options_ind);

  NumericVector out = NumericVector::create(
    Named("gsolve")        = (double)_mem.gsolve_total * sizeof(double),
    Named("gsolve_n0")     = (double)_mem.n0           * sizeof(double),
    Named("gon")           = (double)_mem.gon_total    * sizeof(int),
    Named("gall_times")    = b_gall_times,
    Named("gevid")         = b_gevid,
    Named("gcov")          = b_gcov,
    Named("gpars")         = b_gpars,
    Named("gomega")        = b_gomega,
    Named("gsigma")        = b_gsigma,
    Named("gall_timesS")   = b_gall_timesS,
    Named("ordId")         = b_ordId,
    Named("gInfusionRate") = b_gInfRate,
    Named("inds_global")   = b_inds,
    Named("sizeofInd")    = (double)sizeof(rx_solving_options_ind),
    Named("rxLlikSaveSize")= (double)rxLlikSaveSize);

  return out;
}
